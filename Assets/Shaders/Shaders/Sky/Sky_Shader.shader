Shader "Skybox_Skyshader"
{
	Properties
	{
		[NonModifiableTextureData] [NoScaleOffset] _Texture2DAsset_98667D49_Out("Texture2D", 2D) = "white" {}
	[NonModifiableTextureData][NoScaleOffset] _Texture2DAsset_673071C5_Out("Texture2D", 2D) = "white" {}
	[NonModifiableTextureData][NoScaleOffset] _Texture2DAsset_23DA9093_Out("Texture2D", 2D) = "white" {}
	[NonModifiableTextureData][NoScaleOffset] _Texture2DAsset_B55065F0_Out("Texture2D", 2D) = "white" {}

	}
		SubShader
	{
		Tags
	{
		
		"RenderType" = "Background"
		"Queue" = "Background"
	}

		Pass
	{
		// based on HDUnlitPassForward.template
		
		

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		

		Cull off

	

		ZWrite off

		// Default Stencil


		//-------------------------------------------------------------------------------------
		// End Render Modes
		//-------------------------------------------------------------------------------------

		HLSLPROGRAM

#pragma target 4.5
#pragma only_renderers d3d11 ps4 xboxone vulkan metal switch
		//#pragma enable_d3d11_debug_symbols

		//-------------------------------------------------------------------------------------
		// Variant Definitions (active field translations to HDRP defines)
		//-------------------------------------------------------------------------------------
		//                               #define _ALPHATEST_ON 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_UNLIT // Need to be define before including Material.hlsl

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_DEPTH_ONLY
		// ACTIVE FIELDS:
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
		//                                      #define ATTRIBUTES_NEED_NORMAL
		//                                      #define ATTRIBUTES_NEED_TANGENT
		//                                      #define ATTRIBUTES_NEED_TEXCOORD0
		//                                      #define ATTRIBUTES_NEED_TEXCOORD1
		//                                      #define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
		//                                      #define ATTRIBUTES_NEED_COLOR
		//                                      #define VARYINGS_NEED_POSITION_WS
		//                                      #define VARYINGS_NEED_TANGENT_TO_WORLD
		//                                      #define VARYINGS_NEED_TEXCOORD0
		//                                      #define VARYINGS_NEED_TEXCOORD1
		//                                      #define VARYINGS_NEED_TEXCOORD2
		//                                      #define VARYINGS_NEED_TEXCOORD3
		//                                      #define VARYINGS_NEED_COLOR
		//                                      #define VARYINGS_NEED_CULLFACE
		//                                      #define HAVE_MESH_MODIFICATION

		//-------------------------------------------------------------------------------------
		// End Defines
		//-------------------------------------------------------------------------------------

#include "ShaderGraphLibrary/Functions.hlsl"
#include "HDRP/ShaderVariables.hlsl"
#ifdef DEBUG_DISPLAY
#include "HDRP/Debug/DebugDisplay.hlsl"
#endif

#if (SHADERPASS == SHADERPASS_FORWARD)
		// used for shaders that want to do lighting (and materials)
#include "HDRP/Lighting/Lighting.hlsl"
#else
		// used for shaders that don't need lighting
#include "HDRP/Material/Material.hlsl"
#endif
#include "HDRP/Material/MaterialUtilities.hlsl"

		// this function assumes the bitangent flip is encoded in tangentWS.w
		// TODO: move this function to HDRP shared file, once we merge with HDRP repo
		float3x3 BuildWorldToTangent(float4 tangentWS, float3 normalWS)
	{
		// tangentWS must not be normalized (mikkts requirement)

		// Normalize normalWS vector but keep the renormFactor to apply it to bitangent and tangent
		float3 unnormalizedNormalWS = normalWS;
		float renormFactor = 1.0 / length(unnormalizedNormalWS);

		// bitangent on the fly option in xnormal to reduce vertex shader outputs.
		// this is the mikktspace transformation (must use unnormalized attributes)
		float3x3 worldToTangent = CreateWorldToTangent(unnormalizedNormalWS, tangentWS.xyz, tangentWS.w > 0.0 ? 1.0 : -1.0);

		// surface gradient based formulation requires a unit length initial normal. We can maintain compliance with mikkts
		// by uniformly scaling all 3 vectors since normalization of the perturbed normal will cancel it.
		worldToTangent[0] = worldToTangent[0] * renormFactor;
		worldToTangent[1] = worldToTangent[1] * renormFactor;
		worldToTangent[2] = worldToTangent[2] * renormFactor;		// normalizes the interpolated vertex normal
		return worldToTangent;
	}

	//-------------------------------------------------------------------------------------
	// Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------
	struct AttributesMesh {
		float3 positionOS : POSITION;
	};
	struct VaryingsMeshToPS {
		float4 positionCS : SV_Position;
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		return output;
	}
	struct PackedVaryingsMeshToDS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float3 interp01 : TEXCOORD1; // auto-packed
	};
	PackedVaryingsMeshToDS PackVaryingsMeshToDS(VaryingsMeshToDS input)
	{
		PackedVaryingsMeshToDS output;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xyz = input.normalWS;
		return output;
	}
	VaryingsMeshToDS UnpackVaryingsMeshToDS(PackedVaryingsMeshToDS input)
	{
		VaryingsMeshToDS output;
		output.positionRWS = input.interp00.xyz;
		output.normalWS = input.interp01.xyz;
		return output;
	}
	//-------------------------------------------------------------------------------------
	// End Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------

	//-------------------------------------------------------------------------------------
	// Graph generated code
	//-------------------------------------------------------------------------------------
	// Shared Graph Properties (uniform inputs)
	// Pixel Graph Inputs
	struct SurfaceDescriptionInputs {
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions
	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		surface.Alpha = 1;
		surface.AlphaClipThreshold = 0;
		return surface;
	}

	//-------------------------------------------------------------------------------------
	// End graph generated code
	//-------------------------------------------------------------------------------------

#ifdef HAVE_MESH_MODIFICATION
	// TODO: we should share this between template files somehow
	VertexDescriptionInputs AttributesMeshToVertexDescriptionInputs(AttributesMesh input)
	{
		VertexDescriptionInputs output;
		ZERO_INITIALIZE(VertexDescriptionInputs, output);

		//                                                  output.ObjectSpaceNormal =           input.normalOS;
		//                                                  output.WorldSpaceNormal =            TransformObjectToWorldNormal(input.normalOS);
		//                                                  output.ViewSpaceNormal =             TransformWorldToViewDir(output.WorldSpaceNormal);
		//                                                  output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                  output.ObjectSpaceTangent =          input.tangentOS;
		//                                         		    output.WorldSpaceTangent =           TransformObjectToWorldDir(input.tangentOS.xyz);
		//                                                  output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                  output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                  output.ObjectSpaceBiTangent =        normalize(cross(input.normalOS, input.tangentOS) * (input.tangentOS.w > 0.0f ? 1.0f : -1.0f) * GetOddNegativeScale());
		//                                                  output.WorldSpaceBiTangent =         TransformObjectToWorldDir(output.ObjectSpaceBiTangent);
		//                                                  output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                  output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                  output.ObjectSpacePosition =         input.positionOS;
		//                                                  output.WorldSpacePosition =          GetAbsolutePositionWS(TransformObjectToWorld(input.positionOS));
		//                                                  output.ViewSpacePosition =           TransformWorldToView(output.WorldSpacePosition);
		//                                                  output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                  output.WorldSpaceViewDirection =     GetWorldSpaceNormalizeViewDir(output.WorldSpacePosition);
		//                                                  output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                  output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                  float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                  output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                  output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(output.WorldSpacePosition), _ProjectionParams.x);
		//                                                  output.uv0 =                         float4(input.uv0, 0.0f, 0.0f);
		//                                                  output.uv1 =                         float4(input.uv1, 0.0f, 0.0f);
		//                                                  output.uv2 =                         float4(input.uv2, 0.0f, 0.0f);
		//                                                  output.uv3 =                         float4(input.uv3, 0.0f, 0.0f);
		//                                                  output.VertexColor =                 input.color;

		return output;
	}

	AttributesMesh ApplyMeshModification(AttributesMesh input)
	{
		// build graph inputs
		VertexDescriptionInputs vertexDescriptionInputs = AttributesMeshToVertexDescriptionInputs(input);

		// evaluate vertex graph
		VertexDescription vertexDescription = VertexDescriptionFunction(vertexDescriptionInputs);

		// copy graph output to the results
		//                              input.positionOS = vertexDescription.Position;

		return input;
	}
#endif // HAVE_MESH_MODIFICATION

	// TODO: Do we want to build include functionality for sharing these preprocessed functions across templates?
	FragInputs BuildFragInputs(VaryingsMeshToPS input)
	{
		FragInputs output;
		ZERO_INITIALIZE(FragInputs, output);

		// Init to some default value to make the computer quiet (else it output 'divide by zero' warning even if value is not used).
		// TODO: this is a really poor workaround, but the variable is used in a bunch of places
		// to compute normals which are then passed on elsewhere to compute other values...
		output.worldToTangent = k_identity3x3;
		output.positionSS = input.positionCS;       // input.positionCS is SV_Position

													//                              output.positionRWS = input.positionRWS;
													//                              output.worldToTangent = BuildWorldToTangent(input.tangentWS, input.normalWS);
													//                              output.texCoord0 = input.texCoord0;
													//                              output.texCoord1 = input.texCoord1;
													//                              output.texCoord2 = input.texCoord2;
													//                              output.texCoord3 = input.texCoord3;
													//                              output.color = input.color;
#if SHADER_STAGE_FRAGMENT
													//                              output.isFrontFace = IS_FRONT_VFACE(input.cullFace, true, false);       // TODO: SHADER_STAGE_FRAGMENT only
													//                              // Handle handness of the view matrix (In Unity view matrix default to a determinant of -1)
													//                              // when we render a cubemap the view matrix handness is flipped (due to convention used for cubemap) we have a determinant of +1
													//                              output.isFrontFace = _DetViewMatrix < 0.0 ? output.isFrontFace : !output.isFrontFace;
#endif // SHADER_STAGE_FRAGMENT

		return output;
	}

	SurfaceDescriptionInputs FragInputsToSurfaceDescriptionInputs(FragInputs input, float3 viewWS)
	{
		SurfaceDescriptionInputs output;
		ZERO_INITIALIZE(SurfaceDescriptionInputs, output);

		//                                                   output.WorldSpaceNormal =            normalize(input.worldToTangent[2].xyz);
		//                                                   output.ObjectSpaceNormal =           mul(output.WorldSpaceNormal, (float3x3) unity_ObjectToWorld);      // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.ViewSpaceNormal =             mul(output.WorldSpaceNormal, (float3x3) UNITY_MATRIX_I_V);         // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                   output.WorldSpaceTangent =           input.worldToTangent[0].xyz;
		//                                                   output.ObjectSpaceTangent =          TransformWorldToObjectDir(output.WorldSpaceTangent);
		//                                                   output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                   output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                   output.WorldSpaceBiTangent =         input.worldToTangent[1].xyz;
		//                                                   output.ObjectSpaceBiTangent =        TransformWorldToObjectDir(output.WorldSpaceBiTangent);
		//                                                   output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                   output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                   output.WorldSpaceViewDirection =     normalize(viewWS);
		//                                                   output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                   output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                   float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                   output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                   output.WorldSpacePosition =          GetAbsolutePositionWS(input.positionRWS);
		//                                                   output.ObjectSpacePosition =         TransformWorldToObject(input.positionRWS);
		//                                                   float4 posViewSpace =                TransformWorldToView(input.positionRWS);
		//                                                   output.ViewSpacePosition =           posViewSpace.xyz / posViewSpace.w;
		//                                                   output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                   output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(input.positionRWS), _ProjectionParams.x);

		//                                                   output.uv0 =                         float4(input.texCoord0, 0.0f, 0.0f);
		//                                                   output.uv1 =                         float4(input.texCoord1, 0.0f, 0.0f);
		//                                                   output.uv2 =                         float4(input.texCoord2, 0.0f, 0.0f);
		//                                                   output.uv3 =                         float4(input.texCoord3, 0.0f, 0.0f);

		//                                                   output.VertexColor =                 input.color;

		//                                                   output.FaceSign =    input.isFrontFace;

		return output;
	}

	// existing HDRP code uses the combined function to go directly from packed to frag inputs
	FragInputs UnpackVaryingsMeshToFragInputs(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS unpacked = UnpackVaryingsMeshToPS(input);
		return BuildFragInputs(unpacked);
	}

	void BuildSurfaceData(FragInputs fragInputs, SurfaceDescription surfaceDescription, float3 V, out SurfaceData surfaceData)
	{
		// setup defaults -- these are used if the graph doesn't output a value
		ZERO_INITIALIZE(SurfaceData, surfaceData);

		// copy across graph values, if defined
		//                                       surfaceData.color = surfaceDescription.Color;
	}

	void GetSurfaceAndBuiltinData(FragInputs fragInputs, float3 V, inout PositionInputs posInput, out SurfaceData surfaceData, out BuiltinData builtinData)
	{
		// this applies the double sided tangent space correction -- see 'ApplyDoubleSidedFlipOrMirror()'
		//                      if (!fragInputs.isFrontFace) {
		//                          fragInputs.worldToTangent[1] = -fragInputs.worldToTangent[1];     // bitangent
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                      }

		SurfaceDescriptionInputs surfaceDescriptionInputs = FragInputsToSurfaceDescriptionInputs(fragInputs, V);
		SurfaceDescription surfaceDescription = SurfaceDescriptionFunction(surfaceDescriptionInputs);

		// Perform alpha test very early to save performance (a killed pixel will not sample textures)
		// TODO: split graph evaluation to grab just alpha dependencies first? tricky..
		//              DoAlphaTest(surfaceDescription.Alpha, surfaceDescription.AlphaClipThreshold);

		BuildSurfaceData(fragInputs, surfaceDescription, V, surfaceData);

		// Builtin Data -- we don't call GetBuiltinData(fragInputs, surfaceData, ...)
		// that function assumes there are specific global properties defined
		// for shadergraph shaders, we fill it out here instead
		ZERO_INITIALIZE(BuiltinData, builtinData);

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = float3(0.0, 0.0, 0.0);

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;

		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassDepthOnly.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

		Pass
	{
		// based on HDUnlitPassForward.template
		Name "Forward Unlit"
		Tags{ "LightMode" = "ForwardOnly" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// Default Stencil


		//-------------------------------------------------------------------------------------
		// End Render Modes
		//-------------------------------------------------------------------------------------

		HLSLPROGRAM

#pragma target 4.5
#pragma only_renderers d3d11 ps4 xboxone vulkan metal switch
		//#pragma enable_d3d11_debug_symbols

		//-------------------------------------------------------------------------------------
		// Variant Definitions (active field translations to HDRP defines)
		//-------------------------------------------------------------------------------------
		//                               #define _ALPHATEST_ON 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_UNLIT // Need to be define before including Material.hlsl

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_FORWARD_UNLIT
#pragma multi_compile _ DEBUG_DISPLAY
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.ObjectSpacePosition
		//   SurfaceDescriptionInputs.uv0
		//   SurfaceDescription.Color
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   FragInputs.positionRWS
		//   FragInputs.texCoord0
		//   VaryingsMeshToPS.positionRWS
		//   VaryingsMeshToPS.texCoord0
		//   AttributesMesh.positionOS
		//   AttributesMesh.uv0

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
		//                                      #define ATTRIBUTES_NEED_NORMAL
		//                                      #define ATTRIBUTES_NEED_TANGENT
#define ATTRIBUTES_NEED_TEXCOORD0
		//                                      #define ATTRIBUTES_NEED_TEXCOORD1
		//                                      #define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
		//                                      #define ATTRIBUTES_NEED_COLOR
#define VARYINGS_NEED_POSITION_WS
		//                                      #define VARYINGS_NEED_TANGENT_TO_WORLD
#define VARYINGS_NEED_TEXCOORD0
		//                                      #define VARYINGS_NEED_TEXCOORD1
		//                                      #define VARYINGS_NEED_TEXCOORD2
		//                                      #define VARYINGS_NEED_TEXCOORD3
		//                                      #define VARYINGS_NEED_COLOR
		//                                      #define VARYINGS_NEED_CULLFACE
		//                                      #define HAVE_MESH_MODIFICATION

		//-------------------------------------------------------------------------------------
		// End Defines
		//-------------------------------------------------------------------------------------

#include "ShaderGraphLibrary/Functions.hlsl"
#include "HDRP/ShaderVariables.hlsl"
#ifdef DEBUG_DISPLAY
#include "HDRP/Debug/DebugDisplay.hlsl"
#endif

#if (SHADERPASS == SHADERPASS_FORWARD)
		// used for shaders that want to do lighting (and materials)
#include "HDRP/Lighting/Lighting.hlsl"
#else
		// used for shaders that don't need lighting
#include "HDRP/Material/Material.hlsl"
#endif
#include "HDRP/Material/MaterialUtilities.hlsl"

		// this function assumes the bitangent flip is encoded in tangentWS.w
		// TODO: move this function to HDRP shared file, once we merge with HDRP repo
		float3x3 BuildWorldToTangent(float4 tangentWS, float3 normalWS)
	{
		// tangentWS must not be normalized (mikkts requirement)

		// Normalize normalWS vector but keep the renormFactor to apply it to bitangent and tangent
		float3 unnormalizedNormalWS = normalWS;
		float renormFactor = 1.0 / length(unnormalizedNormalWS);

		// bitangent on the fly option in xnormal to reduce vertex shader outputs.
		// this is the mikktspace transformation (must use unnormalized attributes)
		float3x3 worldToTangent = CreateWorldToTangent(unnormalizedNormalWS, tangentWS.xyz, tangentWS.w > 0.0 ? 1.0 : -1.0);

		// surface gradient based formulation requires a unit length initial normal. We can maintain compliance with mikkts
		// by uniformly scaling all 3 vectors since normalization of the perturbed normal will cancel it.
		worldToTangent[0] = worldToTangent[0] * renormFactor;
		worldToTangent[1] = worldToTangent[1] * renormFactor;
		worldToTangent[2] = worldToTangent[2] * renormFactor;		// normalizes the interpolated vertex normal
		return worldToTangent;
	}

	//-------------------------------------------------------------------------------------
	// Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------
	struct AttributesMesh {
		float3 positionOS : POSITION;
		float2 uv0 : TEXCOORD0; // optional
	};
	struct VaryingsMeshToPS {
		float4 positionCS : SV_Position;
		float3 positionRWS; // optional
		float2 texCoord0; // optional
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float2 interp01 : TEXCOORD1; // auto-packed
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xy = input.texCoord0;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.positionRWS = input.interp00.xyz;
		output.texCoord0 = input.interp01.xy;
		return output;
	}
	struct PackedVaryingsMeshToDS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float3 interp01 : TEXCOORD1; // auto-packed
	};
	PackedVaryingsMeshToDS PackVaryingsMeshToDS(VaryingsMeshToDS input)
	{
		PackedVaryingsMeshToDS output;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xyz = input.normalWS;
		return output;
	}
	VaryingsMeshToDS UnpackVaryingsMeshToDS(PackedVaryingsMeshToDS input)
	{
		VaryingsMeshToDS output;
		output.positionRWS = input.interp00.xyz;
		output.normalWS = input.interp01.xyz;
		return output;
	}
	//-------------------------------------------------------------------------------------
	// End Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------

	//-------------------------------------------------------------------------------------
	// Graph generated code
	//-------------------------------------------------------------------------------------
	// Shared Graph Properties (uniform inputs)
	TEXTURE2D(_Texture2DAsset_98667D49_Out); SAMPLER(sampler_Texture2DAsset_98667D49_Out);
	TEXTURE2D(_Texture2DAsset_673071C5_Out); SAMPLER(sampler_Texture2DAsset_673071C5_Out);
	TEXTURE2D(_Texture2DAsset_23DA9093_Out); SAMPLER(sampler_Texture2DAsset_23DA9093_Out);
	TEXTURE2D(_Texture2DAsset_B55065F0_Out); SAMPLER(sampler_Texture2DAsset_B55065F0_Out);

	// Pixel Graph Inputs
	struct SurfaceDescriptionInputs {
		float3 ObjectSpacePosition; // optional
		float4 uv0; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Color;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions

	void Unity_Multiply_float(float A, float B, out float Out)
	{
		Out = A * B;
	}

	void Unity_Divide_float(float A, float B, out float Out)
	{
		Out = A / B;
	}

	void Unity_TilingAndOffset_float(float2 UV, float2 Tiling, float2 Offset, out float2 Out)
	{
		Out = UV * Tiling + Offset;
	}


	inline float unity_noise_randomValue(float2 uv)
	{
		return frac(sin(dot(uv, float2(12.9898, 78.233)))*43758.5453);
	}

	inline float unity_noise_interpolate(float a, float b, float t)
	{
		return (1.0 - t)*a + (t*b);
	}


	inline float unity_valueNoise(float2 uv)
	{
		float2 i = floor(uv);
		float2 f = frac(uv);
		f = f * f * (3.0 - 2.0 * f);

		uv = abs(frac(uv) - 0.5);
		float2 c0 = i + float2(0.0, 0.0);
		float2 c1 = i + float2(1.0, 0.0);
		float2 c2 = i + float2(0.0, 1.0);
		float2 c3 = i + float2(1.0, 1.0);
		float r0 = unity_noise_randomValue(c0);
		float r1 = unity_noise_randomValue(c1);
		float r2 = unity_noise_randomValue(c2);
		float r3 = unity_noise_randomValue(c3);

		float bottomOfGrid = unity_noise_interpolate(r0, r1, f.x);
		float topOfGrid = unity_noise_interpolate(r2, r3, f.x);
		float t = unity_noise_interpolate(bottomOfGrid, topOfGrid, f.y);
		return t;
	}
	void Unity_SimpleNoise_float(float2 UV, float Scale, out float Out)
	{
		float t = 0.0;
		for (int i = 0; i < 3; i++)
		{
			float freq = pow(2.0, float(i));
			float amp = pow(0.5, float(3 - i));
			t += unity_valueNoise(float2(UV.x*Scale / freq, UV.y*Scale / freq))*amp;
		}
		Out = t;
	}

	void Unity_Multiply_float(float4 A, float4 B, out float4 Out)
	{
		Out = A * B;
	}

	void Unity_Add_float4(float4 A, float4 B, out float4 Out)
	{
		Out = A + B;
	}

	void Unity_Add_float(float A, float B, out float Out)
	{
		Out = A + B;
	}

	void Unity_Lerp_float4(float4 A, float4 B, float4 T, out float4 Out)
	{
		Out = lerp(A, B, T);
	}

	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		float Slider_4D7D48DE = 0;
		float _Multiply_8E0EB4E8_Out;
		Unity_Multiply_float(_Time.y, Slider_4D7D48DE, _Multiply_8E0EB4E8_Out);

		float _Divide_11E474B3_Out;
		Unity_Divide_float(_Multiply_8E0EB4E8_Out, 100, _Divide_11E474B3_Out);
		float2 _Vector2_7A543DBF_Out = float2(_Divide_11E474B3_Out,0);
		float2 _TilingAndOffset_1E768946_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_7A543DBF_Out, _TilingAndOffset_1E768946_Out);
		float4 _SampleTexture2D_CFDEEAD0_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_98667D49_Out, sampler_Texture2DAsset_98667D49_Out, _TilingAndOffset_1E768946_Out);
		float _SampleTexture2D_CFDEEAD0_R = _SampleTexture2D_CFDEEAD0_RGBA.r;
		float _SampleTexture2D_CFDEEAD0_G = _SampleTexture2D_CFDEEAD0_RGBA.g;
		float _SampleTexture2D_CFDEEAD0_B = _SampleTexture2D_CFDEEAD0_RGBA.b;
		float _SampleTexture2D_CFDEEAD0_A = _SampleTexture2D_CFDEEAD0_RGBA.a;
		float _SimpleNoise_41D0056A_Out;
		Unity_SimpleNoise_float(IN.uv0.xy, 20, _SimpleNoise_41D0056A_Out);
		float4 _Multiply_1C4652D4_Out;
		Unity_Multiply_float(_SampleTexture2D_CFDEEAD0_RGBA, (_SimpleNoise_41D0056A_Out.xxxx), _Multiply_1C4652D4_Out);

		float Slider_1484E353 = 0;
		float _Multiply_9AE07412_Out;
		Unity_Multiply_float(_Time.y, Slider_1484E353, _Multiply_9AE07412_Out);

		float _Divide_8698388E_Out;
		Unity_Divide_float(_Multiply_9AE07412_Out, 100, _Divide_8698388E_Out);
		float _Multiply_6B1DF87A_Out;
		Unity_Multiply_float(_Divide_8698388E_Out, -1, _Multiply_6B1DF87A_Out);

		float2 _Vector2_1F0F9C96_Out = float2(_Multiply_6B1DF87A_Out,0);
		float2 _TilingAndOffset_2505F55E_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_1F0F9C96_Out, _TilingAndOffset_2505F55E_Out);
		float4 _SampleTexture2D_F655CC41_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_673071C5_Out, sampler_Texture2DAsset_673071C5_Out, _TilingAndOffset_2505F55E_Out);
		float _SampleTexture2D_F655CC41_R = _SampleTexture2D_F655CC41_RGBA.r;
		float _SampleTexture2D_F655CC41_G = _SampleTexture2D_F655CC41_RGBA.g;
		float _SampleTexture2D_F655CC41_B = _SampleTexture2D_F655CC41_RGBA.b;
		float _SampleTexture2D_F655CC41_A = _SampleTexture2D_F655CC41_RGBA.a;
		float4 _Multiply_DA156782_Out;
		Unity_Multiply_float(_SampleTexture2D_F655CC41_RGBA, float4(0.2, 0.2, 0.2, 2), _Multiply_DA156782_Out);

		float4 _Add_D629A317_Out;
		Unity_Add_float4(_Multiply_1C4652D4_Out, _Multiply_DA156782_Out, _Add_D629A317_Out);
		float Slider_A99CA48 = 0;
		float Slider_A0A050A5 = 0;
		float2 _Vector2_6B44C54F_Out = float2(Slider_A99CA48,Slider_A0A050A5);
		float2 _TilingAndOffset_71EC3A55_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_6B44C54F_Out, _TilingAndOffset_71EC3A55_Out);
		float4 _SampleTexture2DLOD_9FBA6BFF_RGBA = SAMPLE_TEXTURE2D_LOD(_Texture2DAsset_23DA9093_Out, sampler_Texture2DAsset_23DA9093_Out, _TilingAndOffset_71EC3A55_Out, 0);
		float _SampleTexture2DLOD_9FBA6BFF_R = _SampleTexture2DLOD_9FBA6BFF_RGBA.r;
		float _SampleTexture2DLOD_9FBA6BFF_G = _SampleTexture2DLOD_9FBA6BFF_RGBA.g;
		float _SampleTexture2DLOD_9FBA6BFF_B = _SampleTexture2DLOD_9FBA6BFF_RGBA.b;
		float _SampleTexture2DLOD_9FBA6BFF_A = _SampleTexture2DLOD_9FBA6BFF_RGBA.a;
		float Slider_450601F2 = 1;
		float4 _Multiply_CCE816F1_Out;
		Unity_Multiply_float(_SampleTexture2DLOD_9FBA6BFF_RGBA, (Slider_450601F2.xxxx), _Multiply_CCE816F1_Out);

		float _Vector1_DD9758A3_Out = 1;
		float2 _TilingAndOffset_8080CD6E_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, (_Vector1_DD9758A3_Out.xx), float2 (0,0), _TilingAndOffset_8080CD6E_Out);
		float4 _SampleTexture2D_B1003C9E_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_B55065F0_Out, sampler_Texture2DAsset_B55065F0_Out, _TilingAndOffset_8080CD6E_Out);
		float _SampleTexture2D_B1003C9E_R = _SampleTexture2D_B1003C9E_RGBA.r;
		float _SampleTexture2D_B1003C9E_G = _SampleTexture2D_B1003C9E_RGBA.g;
		float _SampleTexture2D_B1003C9E_B = _SampleTexture2D_B1003C9E_RGBA.b;
		float _SampleTexture2D_B1003C9E_A = _SampleTexture2D_B1003C9E_RGBA.a;
		float _Split_CD3E5850_R = IN.ObjectSpacePosition[0];
		float _Split_CD3E5850_G = IN.ObjectSpacePosition[1];
		float _Split_CD3E5850_B = IN.ObjectSpacePosition[2];
		float _Split_CD3E5850_A = 0;
		float _Multiply_7D782456_Out;
		Unity_Multiply_float(_Split_CD3E5850_G, 2, _Multiply_7D782456_Out);

		float _Add_BFD29D8A_Out;
		Unity_Add_float(_Multiply_7D782456_Out, 0.4, _Add_BFD29D8A_Out);
		float4 _Multiply_A2239156_Out;
		Unity_Multiply_float(_SampleTexture2D_B1003C9E_RGBA, (_Add_BFD29D8A_Out.xxxx), _Multiply_A2239156_Out);

		float Slider_604A9E8A = 1;
		float4 _Multiply_64C7803F_Out;
		Unity_Multiply_float(_Multiply_A2239156_Out, (Slider_604A9E8A.xxxx), _Multiply_64C7803F_Out);

		float4 Color_50E49DCB = IsGammaSpace() ? float4(0.02331789, 0.03093999, 0.03773582, 0) : float4(SRGBToLinear(float3(0.02331789, 0.03093999, 0.03773582)), 0);
		float4 Color_7146D3F5 = IsGammaSpace() ? float4(0.0969206, 0.1370686, 0.1698113, 0) : float4(SRGBToLinear(float3(0.0969206, 0.1370686, 0.1698113)), 0);
		float4 _Lerp_C3AFD99F_Out;
		Unity_Lerp_float4(Color_50E49DCB, Color_7146D3F5, (_Add_BFD29D8A_Out.xxxx), _Lerp_C3AFD99F_Out);
		float4 _Add_52CABE3F_Out;
		Unity_Add_float4(_Multiply_64C7803F_Out, _Lerp_C3AFD99F_Out, _Add_52CABE3F_Out);
		float4 _Add_47698B0C_Out;
		Unity_Add_float4(_Multiply_CCE816F1_Out, _Add_52CABE3F_Out, _Add_47698B0C_Out);
		float4 _Add_58149DD0_Out;
		Unity_Add_float4(_Add_D629A317_Out, _Add_47698B0C_Out, _Add_58149DD0_Out);
		surface.Color = (_Add_58149DD0_Out.xyz);
		surface.Alpha = 1;
		surface.AlphaClipThreshold = 0;
		return surface;
	}

	//-------------------------------------------------------------------------------------
	// End graph generated code
	//-------------------------------------------------------------------------------------

#ifdef HAVE_MESH_MODIFICATION
	// TODO: we should share this between template files somehow
	VertexDescriptionInputs AttributesMeshToVertexDescriptionInputs(AttributesMesh input)
	{
		VertexDescriptionInputs output;
		ZERO_INITIALIZE(VertexDescriptionInputs, output);

		//                                                  output.ObjectSpaceNormal =           input.normalOS;
		//                                                  output.WorldSpaceNormal =            TransformObjectToWorldNormal(input.normalOS);
		//                                                  output.ViewSpaceNormal =             TransformWorldToViewDir(output.WorldSpaceNormal);
		//                                                  output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                  output.ObjectSpaceTangent =          input.tangentOS;
		//                                         		    output.WorldSpaceTangent =           TransformObjectToWorldDir(input.tangentOS.xyz);
		//                                                  output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                  output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                  output.ObjectSpaceBiTangent =        normalize(cross(input.normalOS, input.tangentOS) * (input.tangentOS.w > 0.0f ? 1.0f : -1.0f) * GetOddNegativeScale());
		//                                                  output.WorldSpaceBiTangent =         TransformObjectToWorldDir(output.ObjectSpaceBiTangent);
		//                                                  output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                  output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                  output.ObjectSpacePosition =         input.positionOS;
		//                                                  output.WorldSpacePosition =          GetAbsolutePositionWS(TransformObjectToWorld(input.positionOS));
		//                                                  output.ViewSpacePosition =           TransformWorldToView(output.WorldSpacePosition);
		//                                                  output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                  output.WorldSpaceViewDirection =     GetWorldSpaceNormalizeViewDir(output.WorldSpacePosition);
		//                                                  output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                  output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                  float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                  output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                  output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(output.WorldSpacePosition), _ProjectionParams.x);
		//                                                  output.uv0 =                         float4(input.uv0, 0.0f, 0.0f);
		//                                                  output.uv1 =                         float4(input.uv1, 0.0f, 0.0f);
		//                                                  output.uv2 =                         float4(input.uv2, 0.0f, 0.0f);
		//                                                  output.uv3 =                         float4(input.uv3, 0.0f, 0.0f);
		//                                                  output.VertexColor =                 input.color;

		return output;
	}

	AttributesMesh ApplyMeshModification(AttributesMesh input)
	{
		// build graph inputs
		VertexDescriptionInputs vertexDescriptionInputs = AttributesMeshToVertexDescriptionInputs(input);

		// evaluate vertex graph
		VertexDescription vertexDescription = VertexDescriptionFunction(vertexDescriptionInputs);

		// copy graph output to the results
		//                              input.positionOS = vertexDescription.Position;

		return input;
	}
#endif // HAVE_MESH_MODIFICATION

	// TODO: Do we want to build include functionality for sharing these preprocessed functions across templates?
	FragInputs BuildFragInputs(VaryingsMeshToPS input)
	{
		FragInputs output;
		ZERO_INITIALIZE(FragInputs, output);

		// Init to some default value to make the computer quiet (else it output 'divide by zero' warning even if value is not used).
		// TODO: this is a really poor workaround, but the variable is used in a bunch of places
		// to compute normals which are then passed on elsewhere to compute other values...
		output.worldToTangent = k_identity3x3;
		output.positionSS = input.positionCS;       // input.positionCS is SV_Position

		output.positionRWS = input.positionRWS;
		//                              output.worldToTangent = BuildWorldToTangent(input.tangentWS, input.normalWS);
		output.texCoord0 = input.texCoord0;
		//                              output.texCoord1 = input.texCoord1;
		//                              output.texCoord2 = input.texCoord2;
		//                              output.texCoord3 = input.texCoord3;
		//                              output.color = input.color;
#if SHADER_STAGE_FRAGMENT
		//                              output.isFrontFace = IS_FRONT_VFACE(input.cullFace, true, false);       // TODO: SHADER_STAGE_FRAGMENT only
		//                              // Handle handness of the view matrix (In Unity view matrix default to a determinant of -1)
		//                              // when we render a cubemap the view matrix handness is flipped (due to convention used for cubemap) we have a determinant of +1
		//                              output.isFrontFace = _DetViewMatrix < 0.0 ? output.isFrontFace : !output.isFrontFace;
#endif // SHADER_STAGE_FRAGMENT

		return output;
	}

	SurfaceDescriptionInputs FragInputsToSurfaceDescriptionInputs(FragInputs input, float3 viewWS)
	{
		SurfaceDescriptionInputs output;
		ZERO_INITIALIZE(SurfaceDescriptionInputs, output);

		//                                                   output.WorldSpaceNormal =            normalize(input.worldToTangent[2].xyz);
		//                                                   output.ObjectSpaceNormal =           mul(output.WorldSpaceNormal, (float3x3) unity_ObjectToWorld);      // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.ViewSpaceNormal =             mul(output.WorldSpaceNormal, (float3x3) UNITY_MATRIX_I_V);         // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                   output.WorldSpaceTangent =           input.worldToTangent[0].xyz;
		//                                                   output.ObjectSpaceTangent =          TransformWorldToObjectDir(output.WorldSpaceTangent);
		//                                                   output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                   output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                   output.WorldSpaceBiTangent =         input.worldToTangent[1].xyz;
		//                                                   output.ObjectSpaceBiTangent =        TransformWorldToObjectDir(output.WorldSpaceBiTangent);
		//                                                   output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                   output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                   output.WorldSpaceViewDirection =     normalize(viewWS);
		//                                                   output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                   output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                   float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                   output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                   output.WorldSpacePosition =          GetAbsolutePositionWS(input.positionRWS);
		output.ObjectSpacePosition = TransformWorldToObject(input.positionRWS);
		//                                                   float4 posViewSpace =                TransformWorldToView(input.positionRWS);
		//                                                   output.ViewSpacePosition =           posViewSpace.xyz / posViewSpace.w;
		//                                                   output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                   output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(input.positionRWS), _ProjectionParams.x);

		output.uv0 = float4(input.texCoord0, 0.0f, 0.0f);
		//                                                   output.uv1 =                         float4(input.texCoord1, 0.0f, 0.0f);
		//                                                   output.uv2 =                         float4(input.texCoord2, 0.0f, 0.0f);
		//                                                   output.uv3 =                         float4(input.texCoord3, 0.0f, 0.0f);

		//                                                   output.VertexColor =                 input.color;

		//                                                   output.FaceSign =    input.isFrontFace;

		return output;
	}

	// existing HDRP code uses the combined function to go directly from packed to frag inputs
	FragInputs UnpackVaryingsMeshToFragInputs(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS unpacked = UnpackVaryingsMeshToPS(input);
		return BuildFragInputs(unpacked);
	}

	void BuildSurfaceData(FragInputs fragInputs, SurfaceDescription surfaceDescription, float3 V, out SurfaceData surfaceData)
	{
		// setup defaults -- these are used if the graph doesn't output a value
		ZERO_INITIALIZE(SurfaceData, surfaceData);

		// copy across graph values, if defined
		surfaceData.color = surfaceDescription.Color;
	}

	void GetSurfaceAndBuiltinData(FragInputs fragInputs, float3 V, inout PositionInputs posInput, out SurfaceData surfaceData, out BuiltinData builtinData)
	{
		// this applies the double sided tangent space correction -- see 'ApplyDoubleSidedFlipOrMirror()'
		//                      if (!fragInputs.isFrontFace) {
		//                          fragInputs.worldToTangent[1] = -fragInputs.worldToTangent[1];     // bitangent
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                      }

		SurfaceDescriptionInputs surfaceDescriptionInputs = FragInputsToSurfaceDescriptionInputs(fragInputs, V);
		SurfaceDescription surfaceDescription = SurfaceDescriptionFunction(surfaceDescriptionInputs);

		// Perform alpha test very early to save performance (a killed pixel will not sample textures)
		// TODO: split graph evaluation to grab just alpha dependencies first? tricky..
		//              DoAlphaTest(surfaceDescription.Alpha, surfaceDescription.AlphaClipThreshold);

		BuildSurfaceData(fragInputs, surfaceDescription, V, surfaceData);

		// Builtin Data -- we don't call GetBuiltinData(fragInputs, surfaceData, ...)
		// that function assumes there are specific global properties defined
		// for shadergraph shaders, we fill it out here instead
		ZERO_INITIALIZE(BuiltinData, builtinData);

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = float3(0.0, 0.0, 0.0);

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;

		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassForwardUnlit.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

		Pass
	{
		// based on HDUnlitPassForward.template
		Name "META"
		Tags{ "LightMode" = "Meta" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Off

		ZTest LEqual

		ZWrite On

		// Default Stencil


		//-------------------------------------------------------------------------------------
		// End Render Modes
		//-------------------------------------------------------------------------------------

		HLSLPROGRAM

#pragma target 4.5
#pragma only_renderers d3d11 ps4 xboxone vulkan metal switch
		//#pragma enable_d3d11_debug_symbols

		//-------------------------------------------------------------------------------------
		// Variant Definitions (active field translations to HDRP defines)
		//-------------------------------------------------------------------------------------
		//                               #define _ALPHATEST_ON 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_UNLIT // Need to be define before including Material.hlsl

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_LIGHT_TRANSPORT
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.ObjectSpacePosition
		//   SurfaceDescriptionInputs.uv0
		//   SurfaceDescription.Color
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   AttributesMesh.normalOS
		//   AttributesMesh.tangentOS
		//   AttributesMesh.uv0
		//   AttributesMesh.uv1
		//   AttributesMesh.color
		//   AttributesMesh.uv2
		//   FragInputs.positionRWS
		//   FragInputs.texCoord0
		//   VaryingsMeshToPS.positionRWS
		//   VaryingsMeshToPS.texCoord0
		//   AttributesMesh.positionOS

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
#define ATTRIBUTES_NEED_NORMAL
#define ATTRIBUTES_NEED_TANGENT
#define ATTRIBUTES_NEED_TEXCOORD0
#define ATTRIBUTES_NEED_TEXCOORD1
#define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
#define ATTRIBUTES_NEED_COLOR
#define VARYINGS_NEED_POSITION_WS
		//                                      #define VARYINGS_NEED_TANGENT_TO_WORLD
#define VARYINGS_NEED_TEXCOORD0
		//                                      #define VARYINGS_NEED_TEXCOORD1
		//                                      #define VARYINGS_NEED_TEXCOORD2
		//                                      #define VARYINGS_NEED_TEXCOORD3
		//                                      #define VARYINGS_NEED_COLOR
		//                                      #define VARYINGS_NEED_CULLFACE
		//                                      #define HAVE_MESH_MODIFICATION

		//-------------------------------------------------------------------------------------
		// End Defines
		//-------------------------------------------------------------------------------------

#include "ShaderGraphLibrary/Functions.hlsl"
#include "HDRP/ShaderVariables.hlsl"
#ifdef DEBUG_DISPLAY
#include "HDRP/Debug/DebugDisplay.hlsl"
#endif

#if (SHADERPASS == SHADERPASS_FORWARD)
		// used for shaders that want to do lighting (and materials)
#include "HDRP/Lighting/Lighting.hlsl"
#else
		// used for shaders that don't need lighting
#include "HDRP/Material/Material.hlsl"
#endif
#include "HDRP/Material/MaterialUtilities.hlsl"

		// this function assumes the bitangent flip is encoded in tangentWS.w
		// TODO: move this function to HDRP shared file, once we merge with HDRP repo
		float3x3 BuildWorldToTangent(float4 tangentWS, float3 normalWS)
	{
		// tangentWS must not be normalized (mikkts requirement)

		// Normalize normalWS vector but keep the renormFactor to apply it to bitangent and tangent
		float3 unnormalizedNormalWS = normalWS;
		float renormFactor = 1.0 / length(unnormalizedNormalWS);

		// bitangent on the fly option in xnormal to reduce vertex shader outputs.
		// this is the mikktspace transformation (must use unnormalized attributes)
		float3x3 worldToTangent = CreateWorldToTangent(unnormalizedNormalWS, tangentWS.xyz, tangentWS.w > 0.0 ? 1.0 : -1.0);

		// surface gradient based formulation requires a unit length initial normal. We can maintain compliance with mikkts
		// by uniformly scaling all 3 vectors since normalization of the perturbed normal will cancel it.
		worldToTangent[0] = worldToTangent[0] * renormFactor;
		worldToTangent[1] = worldToTangent[1] * renormFactor;
		worldToTangent[2] = worldToTangent[2] * renormFactor;		// normalizes the interpolated vertex normal
		return worldToTangent;
	}

	//-------------------------------------------------------------------------------------
	// Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------
	struct AttributesMesh {
		float3 positionOS : POSITION;
		float3 normalOS : NORMAL; // optional
		float4 tangentOS : TANGENT; // optional
		float2 uv0 : TEXCOORD0; // optional
		float2 uv1 : TEXCOORD1; // optional
		float2 uv2 : TEXCOORD2; // optional
		float4 color : COLOR; // optional
	};
	struct VaryingsMeshToPS {
		float4 positionCS : SV_Position;
		float3 positionRWS; // optional
		float2 texCoord0; // optional
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float2 interp01 : TEXCOORD1; // auto-packed
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xy = input.texCoord0;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.positionRWS = input.interp00.xyz;
		output.texCoord0 = input.interp01.xy;
		return output;
	}
	struct PackedVaryingsMeshToDS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float3 interp01 : TEXCOORD1; // auto-packed
	};
	PackedVaryingsMeshToDS PackVaryingsMeshToDS(VaryingsMeshToDS input)
	{
		PackedVaryingsMeshToDS output;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xyz = input.normalWS;
		return output;
	}
	VaryingsMeshToDS UnpackVaryingsMeshToDS(PackedVaryingsMeshToDS input)
	{
		VaryingsMeshToDS output;
		output.positionRWS = input.interp00.xyz;
		output.normalWS = input.interp01.xyz;
		return output;
	}
	//-------------------------------------------------------------------------------------
	// End Interpolator Packing And Struct Declarations
	//-------------------------------------------------------------------------------------

	//-------------------------------------------------------------------------------------
	// Graph generated code
	//-------------------------------------------------------------------------------------
	// Shared Graph Properties (uniform inputs)
	TEXTURE2D(_Texture2DAsset_98667D49_Out); SAMPLER(sampler_Texture2DAsset_98667D49_Out);
	TEXTURE2D(_Texture2DAsset_673071C5_Out); SAMPLER(sampler_Texture2DAsset_673071C5_Out);
	TEXTURE2D(_Texture2DAsset_23DA9093_Out); SAMPLER(sampler_Texture2DAsset_23DA9093_Out);
	TEXTURE2D(_Texture2DAsset_B55065F0_Out); SAMPLER(sampler_Texture2DAsset_B55065F0_Out);

	// Pixel Graph Inputs
	struct SurfaceDescriptionInputs {
		float3 ObjectSpacePosition; // optional
		float4 uv0; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Color;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions

	void Unity_Multiply_float(float A, float B, out float Out)
	{
		Out = A * B;
	}

	void Unity_Divide_float(float A, float B, out float Out)
	{
		Out = A / B;
	}

	void Unity_TilingAndOffset_float(float2 UV, float2 Tiling, float2 Offset, out float2 Out)
	{
		Out = UV * Tiling + Offset;
	}


	inline float unity_noise_randomValue(float2 uv)
	{
		return frac(sin(dot(uv, float2(12.9898, 78.233)))*43758.5453);
	}

	inline float unity_noise_interpolate(float a, float b, float t)
	{
		return (1.0 - t)*a + (t*b);
	}


	inline float unity_valueNoise(float2 uv)
	{
		float2 i = floor(uv);
		float2 f = frac(uv);
		f = f * f * (3.0 - 2.0 * f);

		uv = abs(frac(uv) - 0.5);
		float2 c0 = i + float2(0.0, 0.0);
		float2 c1 = i + float2(1.0, 0.0);
		float2 c2 = i + float2(0.0, 1.0);
		float2 c3 = i + float2(1.0, 1.0);
		float r0 = unity_noise_randomValue(c0);
		float r1 = unity_noise_randomValue(c1);
		float r2 = unity_noise_randomValue(c2);
		float r3 = unity_noise_randomValue(c3);

		float bottomOfGrid = unity_noise_interpolate(r0, r1, f.x);
		float topOfGrid = unity_noise_interpolate(r2, r3, f.x);
		float t = unity_noise_interpolate(bottomOfGrid, topOfGrid, f.y);
		return t;
	}
	void Unity_SimpleNoise_float(float2 UV, float Scale, out float Out)
	{
		float t = 0.0;
		for (int i = 0; i < 3; i++)
		{
			float freq = pow(2.0, float(i));
			float amp = pow(0.5, float(3 - i));
			t += unity_valueNoise(float2(UV.x*Scale / freq, UV.y*Scale / freq))*amp;
		}
		Out = t;
	}

	void Unity_Multiply_float(float4 A, float4 B, out float4 Out)
	{
		Out = A * B;
	}

	void Unity_Add_float4(float4 A, float4 B, out float4 Out)
	{
		Out = A + B;
	}

	void Unity_Add_float(float A, float B, out float Out)
	{
		Out = A + B;
	}

	void Unity_Lerp_float4(float4 A, float4 B, float4 T, out float4 Out)
	{
		Out = lerp(A, B, T);
	}

	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		float Slider_4D7D48DE = 0;
		float _Multiply_8E0EB4E8_Out;
		Unity_Multiply_float(_Time.y, Slider_4D7D48DE, _Multiply_8E0EB4E8_Out);

		float _Divide_11E474B3_Out;
		Unity_Divide_float(_Multiply_8E0EB4E8_Out, 100, _Divide_11E474B3_Out);
		float2 _Vector2_7A543DBF_Out = float2(_Divide_11E474B3_Out,0);
		float2 _TilingAndOffset_1E768946_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_7A543DBF_Out, _TilingAndOffset_1E768946_Out);
		float4 _SampleTexture2D_CFDEEAD0_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_98667D49_Out, sampler_Texture2DAsset_98667D49_Out, _TilingAndOffset_1E768946_Out);
		float _SampleTexture2D_CFDEEAD0_R = _SampleTexture2D_CFDEEAD0_RGBA.r;
		float _SampleTexture2D_CFDEEAD0_G = _SampleTexture2D_CFDEEAD0_RGBA.g;
		float _SampleTexture2D_CFDEEAD0_B = _SampleTexture2D_CFDEEAD0_RGBA.b;
		float _SampleTexture2D_CFDEEAD0_A = _SampleTexture2D_CFDEEAD0_RGBA.a;
		float _SimpleNoise_41D0056A_Out;
		Unity_SimpleNoise_float(IN.uv0.xy, 20, _SimpleNoise_41D0056A_Out);
		float4 _Multiply_1C4652D4_Out;
		Unity_Multiply_float(_SampleTexture2D_CFDEEAD0_RGBA, (_SimpleNoise_41D0056A_Out.xxxx), _Multiply_1C4652D4_Out);

		float Slider_1484E353 = 0;
		float _Multiply_9AE07412_Out;
		Unity_Multiply_float(_Time.y, Slider_1484E353, _Multiply_9AE07412_Out);

		float _Divide_8698388E_Out;
		Unity_Divide_float(_Multiply_9AE07412_Out, 100, _Divide_8698388E_Out);
		float _Multiply_6B1DF87A_Out;
		Unity_Multiply_float(_Divide_8698388E_Out, -1, _Multiply_6B1DF87A_Out);

		float2 _Vector2_1F0F9C96_Out = float2(_Multiply_6B1DF87A_Out,0);
		float2 _TilingAndOffset_2505F55E_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_1F0F9C96_Out, _TilingAndOffset_2505F55E_Out);
		float4 _SampleTexture2D_F655CC41_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_673071C5_Out, sampler_Texture2DAsset_673071C5_Out, _TilingAndOffset_2505F55E_Out);
		float _SampleTexture2D_F655CC41_R = _SampleTexture2D_F655CC41_RGBA.r;
		float _SampleTexture2D_F655CC41_G = _SampleTexture2D_F655CC41_RGBA.g;
		float _SampleTexture2D_F655CC41_B = _SampleTexture2D_F655CC41_RGBA.b;
		float _SampleTexture2D_F655CC41_A = _SampleTexture2D_F655CC41_RGBA.a;
		float4 _Multiply_DA156782_Out;
		Unity_Multiply_float(_SampleTexture2D_F655CC41_RGBA, float4(0.2, 0.2, 0.2, 2), _Multiply_DA156782_Out);

		float4 _Add_D629A317_Out;
		Unity_Add_float4(_Multiply_1C4652D4_Out, _Multiply_DA156782_Out, _Add_D629A317_Out);
		float Slider_A99CA48 = 0;
		float Slider_A0A050A5 = 0;
		float2 _Vector2_6B44C54F_Out = float2(Slider_A99CA48,Slider_A0A050A5);
		float2 _TilingAndOffset_71EC3A55_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, float2 (1,1), _Vector2_6B44C54F_Out, _TilingAndOffset_71EC3A55_Out);
		float4 _SampleTexture2DLOD_9FBA6BFF_RGBA = SAMPLE_TEXTURE2D_LOD(_Texture2DAsset_23DA9093_Out, sampler_Texture2DAsset_23DA9093_Out, _TilingAndOffset_71EC3A55_Out, 0);
		float _SampleTexture2DLOD_9FBA6BFF_R = _SampleTexture2DLOD_9FBA6BFF_RGBA.r;
		float _SampleTexture2DLOD_9FBA6BFF_G = _SampleTexture2DLOD_9FBA6BFF_RGBA.g;
		float _SampleTexture2DLOD_9FBA6BFF_B = _SampleTexture2DLOD_9FBA6BFF_RGBA.b;
		float _SampleTexture2DLOD_9FBA6BFF_A = _SampleTexture2DLOD_9FBA6BFF_RGBA.a;
		float Slider_450601F2 = 1;
		float4 _Multiply_CCE816F1_Out;
		Unity_Multiply_float(_SampleTexture2DLOD_9FBA6BFF_RGBA, (Slider_450601F2.xxxx), _Multiply_CCE816F1_Out);

		float _Vector1_DD9758A3_Out = 1;
		float2 _TilingAndOffset_8080CD6E_Out;
		Unity_TilingAndOffset_float(IN.uv0.xy, (_Vector1_DD9758A3_Out.xx), float2 (0,0), _TilingAndOffset_8080CD6E_Out);
		float4 _SampleTexture2D_B1003C9E_RGBA = SAMPLE_TEXTURE2D(_Texture2DAsset_B55065F0_Out, sampler_Texture2DAsset_B55065F0_Out, _TilingAndOffset_8080CD6E_Out);
		float _SampleTexture2D_B1003C9E_R = _SampleTexture2D_B1003C9E_RGBA.r;
		float _SampleTexture2D_B1003C9E_G = _SampleTexture2D_B1003C9E_RGBA.g;
		float _SampleTexture2D_B1003C9E_B = _SampleTexture2D_B1003C9E_RGBA.b;
		float _SampleTexture2D_B1003C9E_A = _SampleTexture2D_B1003C9E_RGBA.a;
		float _Split_CD3E5850_R = IN.ObjectSpacePosition[0];
		float _Split_CD3E5850_G = IN.ObjectSpacePosition[1];
		float _Split_CD3E5850_B = IN.ObjectSpacePosition[2];
		float _Split_CD3E5850_A = 0;
		float _Multiply_7D782456_Out;
		Unity_Multiply_float(_Split_CD3E5850_G, 2, _Multiply_7D782456_Out);

		float _Add_BFD29D8A_Out;
		Unity_Add_float(_Multiply_7D782456_Out, 0.4, _Add_BFD29D8A_Out);
		float4 _Multiply_A2239156_Out;
		Unity_Multiply_float(_SampleTexture2D_B1003C9E_RGBA, (_Add_BFD29D8A_Out.xxxx), _Multiply_A2239156_Out);

		float Slider_604A9E8A = 1;
		float4 _Multiply_64C7803F_Out;
		Unity_Multiply_float(_Multiply_A2239156_Out, (Slider_604A9E8A.xxxx), _Multiply_64C7803F_Out);

		float4 Color_50E49DCB = IsGammaSpace() ? float4(0.02331789, 0.03093999, 0.03773582, 0) : float4(SRGBToLinear(float3(0.02331789, 0.03093999, 0.03773582)), 0);
		float4 Color_7146D3F5 = IsGammaSpace() ? float4(0.0969206, 0.1370686, 0.1698113, 0) : float4(SRGBToLinear(float3(0.0969206, 0.1370686, 0.1698113)), 0);
		float4 _Lerp_C3AFD99F_Out;
		Unity_Lerp_float4(Color_50E49DCB, Color_7146D3F5, (_Add_BFD29D8A_Out.xxxx), _Lerp_C3AFD99F_Out);
		float4 _Add_52CABE3F_Out;
		Unity_Add_float4(_Multiply_64C7803F_Out, _Lerp_C3AFD99F_Out, _Add_52CABE3F_Out);
		float4 _Add_47698B0C_Out;
		Unity_Add_float4(_Multiply_CCE816F1_Out, _Add_52CABE3F_Out, _Add_47698B0C_Out);
		float4 _Add_58149DD0_Out;
		Unity_Add_float4(_Add_D629A317_Out, _Add_47698B0C_Out, _Add_58149DD0_Out);
		surface.Color = (_Add_58149DD0_Out.xyz);
		surface.Alpha = 1;
		surface.AlphaClipThreshold = 0;
		return surface;
	}

	//-------------------------------------------------------------------------------------
	// End graph generated code
	//-------------------------------------------------------------------------------------

#ifdef HAVE_MESH_MODIFICATION
	// TODO: we should share this between template files somehow
	VertexDescriptionInputs AttributesMeshToVertexDescriptionInputs(AttributesMesh input)
	{
		VertexDescriptionInputs output;
		ZERO_INITIALIZE(VertexDescriptionInputs, output);

		//                                                  output.ObjectSpaceNormal =           input.normalOS;
		//                                                  output.WorldSpaceNormal =            TransformObjectToWorldNormal(input.normalOS);
		//                                                  output.ViewSpaceNormal =             TransformWorldToViewDir(output.WorldSpaceNormal);
		//                                                  output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                  output.ObjectSpaceTangent =          input.tangentOS;
		//                                         		    output.WorldSpaceTangent =           TransformObjectToWorldDir(input.tangentOS.xyz);
		//                                                  output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                  output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                  output.ObjectSpaceBiTangent =        normalize(cross(input.normalOS, input.tangentOS) * (input.tangentOS.w > 0.0f ? 1.0f : -1.0f) * GetOddNegativeScale());
		//                                                  output.WorldSpaceBiTangent =         TransformObjectToWorldDir(output.ObjectSpaceBiTangent);
		//                                                  output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                  output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                  output.ObjectSpacePosition =         input.positionOS;
		//                                                  output.WorldSpacePosition =          GetAbsolutePositionWS(TransformObjectToWorld(input.positionOS));
		//                                                  output.ViewSpacePosition =           TransformWorldToView(output.WorldSpacePosition);
		//                                                  output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                  output.WorldSpaceViewDirection =     GetWorldSpaceNormalizeViewDir(output.WorldSpacePosition);
		//                                                  output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                  output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                  float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                  output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                  output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(output.WorldSpacePosition), _ProjectionParams.x);
		//                                                  output.uv0 =                         float4(input.uv0, 0.0f, 0.0f);
		//                                                  output.uv1 =                         float4(input.uv1, 0.0f, 0.0f);
		//                                                  output.uv2 =                         float4(input.uv2, 0.0f, 0.0f);
		//                                                  output.uv3 =                         float4(input.uv3, 0.0f, 0.0f);
		//                                                  output.VertexColor =                 input.color;

		return output;
	}

	AttributesMesh ApplyMeshModification(AttributesMesh input)
	{
		// build graph inputs
		VertexDescriptionInputs vertexDescriptionInputs = AttributesMeshToVertexDescriptionInputs(input);

		// evaluate vertex graph
		VertexDescription vertexDescription = VertexDescriptionFunction(vertexDescriptionInputs);

		// copy graph output to the results
		//                              input.positionOS = vertexDescription.Position;

		return input;
	}
#endif // HAVE_MESH_MODIFICATION

	// TODO: Do we want to build include functionality for sharing these preprocessed functions across templates?
	FragInputs BuildFragInputs(VaryingsMeshToPS input)
	{
		FragInputs output;
		ZERO_INITIALIZE(FragInputs, output);

		// Init to some default value to make the computer quiet (else it output 'divide by zero' warning even if value is not used).
		// TODO: this is a really poor workaround, but the variable is used in a bunch of places
		// to compute normals which are then passed on elsewhere to compute other values...
		output.worldToTangent = k_identity3x3;
		output.positionSS = input.positionCS;       // input.positionCS is SV_Position

		output.positionRWS = input.positionRWS;
		//                              output.worldToTangent = BuildWorldToTangent(input.tangentWS, input.normalWS);
		output.texCoord0 = input.texCoord0;
		//                              output.texCoord1 = input.texCoord1;
		//                              output.texCoord2 = input.texCoord2;
		//                              output.texCoord3 = input.texCoord3;
		//                              output.color = input.color;
#if SHADER_STAGE_FRAGMENT
		//                              output.isFrontFace = IS_FRONT_VFACE(input.cullFace, true, false);       // TODO: SHADER_STAGE_FRAGMENT only
		//                              // Handle handness of the view matrix (In Unity view matrix default to a determinant of -1)
		//                              // when we render a cubemap the view matrix handness is flipped (due to convention used for cubemap) we have a determinant of +1
		//                              output.isFrontFace = _DetViewMatrix < 0.0 ? output.isFrontFace : !output.isFrontFace;
#endif // SHADER_STAGE_FRAGMENT

		return output;
	}

	SurfaceDescriptionInputs FragInputsToSurfaceDescriptionInputs(FragInputs input, float3 viewWS)
	{
		SurfaceDescriptionInputs output;
		ZERO_INITIALIZE(SurfaceDescriptionInputs, output);

		//                                                   output.WorldSpaceNormal =            normalize(input.worldToTangent[2].xyz);
		//                                                   output.ObjectSpaceNormal =           mul(output.WorldSpaceNormal, (float3x3) unity_ObjectToWorld);      // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.ViewSpaceNormal =             mul(output.WorldSpaceNormal, (float3x3) UNITY_MATRIX_I_V);         // transposed multiplication by inverse matrix to handle normal scale
		//                                                   output.TangentSpaceNormal =          float3(0.0f, 0.0f, 1.0f);
		//                                                   output.WorldSpaceTangent =           input.worldToTangent[0].xyz;
		//                                                   output.ObjectSpaceTangent =          TransformWorldToObjectDir(output.WorldSpaceTangent);
		//                                                   output.ViewSpaceTangent =            TransformWorldToViewDir(output.WorldSpaceTangent);
		//                                                   output.TangentSpaceTangent =         float3(1.0f, 0.0f, 0.0f);
		//                                                   output.WorldSpaceBiTangent =         input.worldToTangent[1].xyz;
		//                                                   output.ObjectSpaceBiTangent =        TransformWorldToObjectDir(output.WorldSpaceBiTangent);
		//                                                   output.ViewSpaceBiTangent =          TransformWorldToViewDir(output.WorldSpaceBiTangent);
		//                                                   output.TangentSpaceBiTangent =       float3(0.0f, 1.0f, 0.0f);
		//                                                   output.WorldSpaceViewDirection =     normalize(viewWS);
		//                                                   output.ObjectSpaceViewDirection =    TransformWorldToObjectDir(output.WorldSpaceViewDirection);
		//                                                   output.ViewSpaceViewDirection =      TransformWorldToViewDir(output.WorldSpaceViewDirection);
		//                                                   float3x3 tangentSpaceTransform =     float3x3(output.WorldSpaceTangent,output.WorldSpaceBiTangent,output.WorldSpaceNormal);
		//                                                   output.TangentSpaceViewDirection =   mul(tangentSpaceTransform, output.WorldSpaceViewDirection);
		//                                                   output.WorldSpacePosition =          GetAbsolutePositionWS(input.positionRWS);
		output.ObjectSpacePosition = TransformWorldToObject(input.positionRWS);
		//                                                   float4 posViewSpace =                TransformWorldToView(input.positionRWS);
		//                                                   output.ViewSpacePosition =           posViewSpace.xyz / posViewSpace.w;
		//                                                   output.TangentSpacePosition =        float3(0.0f, 0.0f, 0.0f);
		//                                                   output.ScreenPosition =              ComputeScreenPos(TransformWorldToHClip(input.positionRWS), _ProjectionParams.x);

		output.uv0 = float4(input.texCoord0, 0.0f, 0.0f);
		//                                                   output.uv1 =                         float4(input.texCoord1, 0.0f, 0.0f);
		//                                                   output.uv2 =                         float4(input.texCoord2, 0.0f, 0.0f);
		//                                                   output.uv3 =                         float4(input.texCoord3, 0.0f, 0.0f);

		//                                                   output.VertexColor =                 input.color;

		//                                                   output.FaceSign =    input.isFrontFace;

		return output;
	}

	// existing HDRP code uses the combined function to go directly from packed to frag inputs
	FragInputs UnpackVaryingsMeshToFragInputs(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS unpacked = UnpackVaryingsMeshToPS(input);
		return BuildFragInputs(unpacked);
	}

	void BuildSurfaceData(FragInputs fragInputs, SurfaceDescription surfaceDescription, float3 V, out SurfaceData surfaceData)
	{
		// setup defaults -- these are used if the graph doesn't output a value
		ZERO_INITIALIZE(SurfaceData, surfaceData);

		// copy across graph values, if defined
		surfaceData.color = surfaceDescription.Color;
	}

	void GetSurfaceAndBuiltinData(FragInputs fragInputs, float3 V, inout PositionInputs posInput, out SurfaceData surfaceData, out BuiltinData builtinData)
	{
		// this applies the double sided tangent space correction -- see 'ApplyDoubleSidedFlipOrMirror()'
		//                      if (!fragInputs.isFrontFace) {
		//                          fragInputs.worldToTangent[1] = -fragInputs.worldToTangent[1];     // bitangent
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                          fragInputs.worldToTangent[2] = -fragInputs.worldToTangent[2];     // normal
		//                      }

		SurfaceDescriptionInputs surfaceDescriptionInputs = FragInputsToSurfaceDescriptionInputs(fragInputs, V);
		SurfaceDescription surfaceDescription = SurfaceDescriptionFunction(surfaceDescriptionInputs);

		// Perform alpha test very early to save performance (a killed pixel will not sample textures)
		// TODO: split graph evaluation to grab just alpha dependencies first? tricky..
		//              DoAlphaTest(surfaceDescription.Alpha, surfaceDescription.AlphaClipThreshold);

		BuildSurfaceData(fragInputs, surfaceDescription, V, surfaceData);

		// Builtin Data -- we don't call GetBuiltinData(fragInputs, surfaceData, ...)
		// that function assumes there are specific global properties defined
		// for shadergraph shaders, we fill it out here instead
		ZERO_INITIALIZE(BuiltinData, builtinData);

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = float3(0.0, 0.0, 0.0);

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;

		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassLightTransport.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

	}
		FallBack "Hidden/InternalErrorShader"
}
