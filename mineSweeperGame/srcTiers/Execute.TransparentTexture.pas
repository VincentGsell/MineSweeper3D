unit Execute.TransparentTexture;

interface

uses
  System.SysUtils,
  FMX.Types3D,
  FMX.Materials,
  FMX.MaterialSources;

type
  TAlphaLightMaterial = class(TLightMaterial)
  protected
    procedure DoInitialize; override;
  end;

  TLightMaterialSource = class(FMX.MaterialSources.TLightMaterialSource)
  protected
    function CreateMaterial: TMaterial; override;
  end;


implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.D3DCommon,
  Winapi.D3DCompiler;

function DXCompile(const Source: string; Kind: TContextShaderKind; Arch: TContextShaderArch = TContextShaderArch.DX11): TBytes;
var
  Data   : TBytes;
  Target : AnsiString;
  Flags  : Cardinal;
  Code   : ID3DBlob;
  Err    : ID3DBlob;
  Str    : string;
begin
  Data := TEncoding.ANSI.GetBytes(Source);
  case Arch of
    TContextShaderArch.DX11_level_9: Target := '2_0'; // D3D_FEATURE_LEVEL_9_1
    TContextShaderArch.DX10        : Target := '4_0'; // D3D_FEATURE_LEVEL_10_0
    TContextShaderArch.DX11        : Target := '5_0'; // D3D_FEATURE_LEVEL_11_0
  else
    raise Exception.Create('Unsupported architecture');
  end;
  case Kind of
    TContextShaderKind.VertexShader: Target := 'vs_' + Target;
    TContextShaderKind.PixelShader : Target := 'ps_' + Target;
  end;

  Flags := D3DCOMPILE_OPTIMIZATION_LEVEL3
//    or D3DCOMPILE_ENABLE_STRICTNESS
    or D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY
    or D3DCOMPILE_WARNINGS_ARE_ERRORS;

  if D3DCompile(Data, Length(Data), nil, nil, nil, 'main', PAnsiChar(Target), Flags, 0, Code, Err) = 0 then
  begin
    SetLength(Result, Code.GetBufferSize);
    Move(Code.GetBufferPointer^, Result[0], Length(Result));
  end else begin
    SetString(Str, PAnsiChar(Err.GetBufferPointer), Err.GetBufferSize);
    raise Exception.Create('Shader compilation error :'#13 + Str);
  end;
end;

const
  PS = 'float Opacity;'#13
     + 'float Modulation;'#13
     + 'sampler2D texture0;'#13
     + 'struct PSInput {'#13
     + ' float4 Pos: POSITION;'#13
     + ' float4 Color: COLOR0;'#13
     + ' float2 Tex0: TEXCOORD0;'#13
     + '};'#13
     + 'float4 main(PSInput input): COLOR {'#13
     + ' float4 color;'#13
     + ' if (Modulation == 0) color = input.Color;'#13
     + ' if (Modulation == 1) color = tex2D(texture0, input.Tex0);'#13
     + ' if (Modulation == 2) color = input.Color * tex2D(texture0, input.Tex0);'#13
     + ' if (color.a < 0.5) discard;'#13  // <<< discard any pixel with Alpha channel < 0.5, ZBuffer will not be affected !
     + ' return color * Opacity;'#13
     + '}';

{$ENDIF}

const
  ps_GL = 'varying vec4 COLOR0;'#13
        + 'varying vec4 TEX0;'#13
        + 'struct PSInput {'#13
        + ' vec4 _Color;'#13
        + ' vec2 _Tex0;'#13
        + '};'#13
        + 'vec4 _ret_0;'#13
        + 'vec4 _TMP0;'#13
        + 'uniform float _Opacity;'#13
        + 'uniform float _Modulation;'#13
        + 'uniform sampler2D _texture0;'#13
        + 'void main() {'#13
        + ' vec4 _color;'#13
        + ' if (_Modulation == 0.0) _color = COLOR0;'#13
        + ' if (_Modulation == 1.0) _color = texture2D(_texture0, TEX0.xy);'#13
        + ' if (_Modulation == 2.0) _color = COLOR0 * texture2D(_texture0, TEX0.xy);'#13
        + ' if (_color.a < 0.5) discard;'#13  // <<< discard any pixel with Alpha channel < 0.5, ZBuffer will not be affected !
        + ' gl_FragColor = _color*_Opacity;'#13
        + '}';

{ TAlphaLightMaterial }

procedure TAlphaLightMaterial.DoInitialize;
begin
  inherited;

  FPixelShader.LoadFromData('ogouraud.fps', TContextShaderKind.PixelShader, '', [
  {$DEFINE GLSL}
  {$IFDEF MSWINDOWS}
   {$UNDEF GLSL}
  // DX11 uses byte offsets for float variables
    TContextShaderSource.Create(TContextShaderArch.DX11, DXCompile(PS, TContextShaderKind.PixelShader, TContextShaderArch.DX11), [
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 4),    // float: offset 0, 4 bytes
      TContextShaderVariable.Create('Modulation', TContextShaderVariableKind.Float, 4, 4), // float: offset 4, 4 bytes
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)] // texture: index 0, size is ignored
    )
  {$ENDIF}
  {$IFDEF MACOS}
   {$UNDEF GLSL}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
          'float4 color;'+
          'float pointSize [[point_size]];'+
        '};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Opacity [[buffer(0)]],'+
                                       'constant float4 &Modulation [[buffer(1)]],'+
                                       'const texture2d<float> texture0 [[texture(0)]],'+
                                       'const sampler texture0Sampler [[sampler(0)]]) {'+
          'float4 color;'+

          'if (Modulation.x == 0)'+
            'color = in.color;'+
          'else if (Modulation.x == 1)'+
            'color = texture0.sample(texture0Sampler, in.textureCoord);'+
          'else '+
            'color = in.color * texture0.sample(texture0Sampler, in.textureCoord);'+

          'if (color.a < 0.5) discard;' +  // <<< discard any pixel with Alpha channel < 0.5, ZBuffer will not be affected !

          'return color * Opacity.x;'+
        '}'
      ),
      [TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Modulation', TContextShaderVariableKind.Float, 1, 1),
       TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    )
  {$ENDIF}
  {$IFDEF GLSL}
    TContextShaderSource.Create(TContextShaderArch.GLSL, TEncoding.UTF8.GetBytes(ps_GL), [
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('Modulation', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    )
  {$ENDIF}
  ]);
end;


{ TLightMaterialSource }

function TLightMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TAlphaLightMaterial.Create;
end;

end.
