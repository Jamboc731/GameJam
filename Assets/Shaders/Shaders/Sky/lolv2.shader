Shader "PBR Master"
{
	Properties
	{

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
		// based on HDPBRPass.template
	

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull off

		

		ZWrite off

		// Stencil setup for gbuffer
		Stencil
	{
		WriteMask 7
		Ref  2
		Comp Always
		Pass Replace
	}


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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_GBUFFER
#pragma multi_compile _ DEBUG_DISPLAY
#pragma multi_compile _ LIGHTMAP_ON
#pragma multi_compile _ DIRLIGHTMAP_COMBINED
#pragma multi_compile _ DYNAMICLIGHTMAP_ON
#pragma multi_compile _ SHADOWS_SHADOWMASK
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
		//   SurfaceDescription.Albedo
		//   SurfaceDescription.Normal
		//   SurfaceDescription.Metallic
		//   SurfaceDescription.Emission
		//   SurfaceDescription.Smoothness
		//   SurfaceDescription.Occlusion
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   FragInputs.worldToTangent
		//   FragInputs.positionRWS
		//   FragInputs.texCoord1
		//   FragInputs.texCoord2
		//   VaryingsMeshToPS.tangentWS
		//   VaryingsMeshToPS.normalWS
		//   VaryingsMeshToPS.positionRWS
		//   VaryingsMeshToPS.texCoord1
		//   VaryingsMeshToPS.texCoord2
		//   AttributesMesh.tangentOS
		//   AttributesMesh.normalOS
		//   AttributesMesh.positionOS
		//   AttributesMesh.uv1
		//   AttributesMesh.uv2

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
#define ATTRIBUTES_NEED_NORMAL
#define ATTRIBUTES_NEED_TANGENT
		//                                      #define ATTRIBUTES_NEED_TEXCOORD0
#define ATTRIBUTES_NEED_TEXCOORD1
#define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
		//                                      #define ATTRIBUTES_NEED_COLOR
#define VARYINGS_NEED_POSITION_WS
#define VARYINGS_NEED_TANGENT_TO_WORLD
		//                                      #define VARYINGS_NEED_TEXCOORD0
#define VARYINGS_NEED_TEXCOORD1
#define VARYINGS_NEED_TEXCOORD2
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
		float2 uv1 : TEXCOORD1; // optional
		float2 uv2 : TEXCOORD2; // optional
	};
	struct VaryingsMeshToPS {
		float4 positionCS : SV_Position;
		float3 positionRWS; // optional
		float3 normalWS; // optional
		float4 tangentWS; // optional
		float2 texCoord1; // optional
		float2 texCoord2; // optional
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float3 interp01 : TEXCOORD1; // auto-packed
		float4 interp02 : TEXCOORD2; // auto-packed
		float4 interp03 : TEXCOORD3; // auto-packed
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.interp00.xyz = input.positionRWS;
		output.interp01.xyz = input.normalWS;
		output.interp02.xyzw = input.tangentWS;
		output.interp03.xy = input.texCoord1;
		output.interp03.zw = input.texCoord2;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.positionRWS = input.interp00.xyz;
		output.normalWS = input.interp01.xyz;
		output.tangentWS = input.interp02.xyzw;
		output.texCoord1 = input.interp03.xy;
		output.texCoord2 = input.interp03.zw;
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
		float3 TangentSpaceNormal; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Albedo;
		float3 Normal;
		float Metallic;
		float3 Emission;
		float Smoothness;
		float Occlusion;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions
	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		surface.Albedo = IsGammaSpace() ? float3(0, 0.4333022, 1) : SRGBToLinear(float3(0, 0.4333022, 1));
		surface.Normal = IN.TangentSpaceNormal;
		surface.Metallic = 0;
		surface.Emission = IsGammaSpace() ? float3(0, 0, 0) : SRGBToLinear(float3(0, 0, 0));
		surface.Smoothness = 0.5;
		surface.Occlusion = 1;
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
		output.worldToTangent = BuildWorldToTangent(input.tangentWS, input.normalWS);
		//                              output.texCoord0 = input.texCoord0;
		output.texCoord1 = input.texCoord1;
		output.texCoord2 = input.texCoord2;
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		surfaceData.baseColor = surfaceDescription.Albedo;
		surfaceData.perceptualSmoothness = surfaceDescription.Smoothness;
		surfaceData.ambientOcclusion = surfaceDescription.Occlusion;
		surfaceData.metallic = surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		normalTS = surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		builtinData.emissiveColor = surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassGBuffer.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

		Pass
	{
		// based on HDPBRPass.template
		Name "GBufferWithPrepass"
		Tags{ "LightMode" = "GBufferWithPrepass" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// Stencil setup for GBufferWithPrepass
		Stencil
	{
		WriteMask 7
		Ref  2
		Comp Always
		Pass Replace
	}


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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_GBUFFER
#pragma multi_compile _ DEBUG_DISPLAY
#pragma multi_compile _ LIGHTMAP_ON
#pragma multi_compile _ DIRLIGHTMAP_COMBINED
#pragma multi_compile _ DYNAMICLIGHTMAP_ON
#pragma multi_compile _ SHADOWS_SHADOWMASK
#define SHADERPASS_GBUFFER_BYPASS_ALPHA_TEST
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
		//   SurfaceDescription.Albedo
		//   SurfaceDescription.Normal
		//   SurfaceDescription.Metallic
		//   SurfaceDescription.Emission
		//   SurfaceDescription.Smoothness
		//   SurfaceDescription.Occlusion
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
		float3 TangentSpaceNormal; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Albedo;
		float3 Normal;
		float Metallic;
		float3 Emission;
		float Smoothness;
		float Occlusion;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions
	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		surface.Albedo = IsGammaSpace() ? float3(0, 0.4333022, 1) : SRGBToLinear(float3(0, 0.4333022, 1));
		surface.Normal = IN.TangentSpaceNormal;
		surface.Metallic = 0;
		surface.Emission = IsGammaSpace() ? float3(0, 0, 0) : SRGBToLinear(float3(0, 0, 0));
		surface.Smoothness = 0.5;
		surface.Occlusion = 1;
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		surfaceData.baseColor = surfaceDescription.Albedo;
		surfaceData.perceptualSmoothness = surfaceDescription.Smoothness;
		surfaceData.ambientOcclusion = surfaceDescription.Occlusion;
		surfaceData.metallic = surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		normalTS = surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		builtinData.emissiveColor = surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassGBuffer.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

		Pass
	{
		// based on HDPBRPass.template
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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_LIGHT_TRANSPORT
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
		//   SurfaceDescription.Albedo
		//   SurfaceDescription.Normal
		//   SurfaceDescription.Metallic
		//   SurfaceDescription.Emission
		//   SurfaceDescription.Smoothness
		//   SurfaceDescription.Occlusion
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   AttributesMesh.normalOS
		//   AttributesMesh.tangentOS
		//   AttributesMesh.uv0
		//   AttributesMesh.uv1
		//   AttributesMesh.color
		//   AttributesMesh.uv2

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
#define ATTRIBUTES_NEED_NORMAL
#define ATTRIBUTES_NEED_TANGENT
#define ATTRIBUTES_NEED_TEXCOORD0
#define ATTRIBUTES_NEED_TEXCOORD1
#define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
#define ATTRIBUTES_NEED_COLOR
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
		float3 normalOS : NORMAL; // optional
		float4 tangentOS : TANGENT; // optional
		float2 uv0 : TEXCOORD0; // optional
		float2 uv1 : TEXCOORD1; // optional
		float2 uv2 : TEXCOORD2; // optional
		float4 color : COLOR; // optional
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
		float3 TangentSpaceNormal; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Albedo;
		float3 Normal;
		float Metallic;
		float3 Emission;
		float Smoothness;
		float Occlusion;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions
	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		surface.Albedo = IsGammaSpace() ? float3(0, 0.4333022, 1) : SRGBToLinear(float3(0, 0.4333022, 1));
		surface.Normal = IN.TangentSpaceNormal;
		surface.Metallic = 0;
		surface.Emission = IsGammaSpace() ? float3(0, 0, 0) : SRGBToLinear(float3(0, 0, 0));
		surface.Smoothness = 0.5;
		surface.Occlusion = 1;
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		surfaceData.baseColor = surfaceDescription.Albedo;
		surfaceData.perceptualSmoothness = surfaceDescription.Smoothness;
		surfaceData.ambientOcclusion = surfaceDescription.Occlusion;
		surfaceData.metallic = surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		normalTS = surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		builtinData.emissiveColor = surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
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

		Pass
	{
		// based on HDPBRPass.template
		Name "ShadowCaster"
		Tags{ "LightMode" = "ShadowCaster" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// Default Stencil

		ColorMask 0

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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_SHADOWS
#define USE_LEGACY_UNITY_MATRIX_VARIABLES
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
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
		float3 TangentSpaceNormal; // optional
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		//                                        surfaceData.baseColor =             surfaceDescription.Albedo;
		//                                        surfaceData.perceptualSmoothness =  surfaceDescription.Smoothness;
		//                                        surfaceData.ambientOcclusion =      surfaceDescription.Occlusion;
		//                                        surfaceData.metallic =              surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		//                                normalTS =                   surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
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
		// based on HDPBRPass.template
		Name "DepthOnly"
		Tags{ "LightMode" = "DepthOnly" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// Default Stencil

		ColorMask 0

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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_DEPTH_ONLY
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
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
		float3 TangentSpaceNormal; // optional
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		//                                        surfaceData.baseColor =             surfaceDescription.Albedo;
		//                                        surfaceData.perceptualSmoothness =  surfaceDescription.Smoothness;
		//                                        surfaceData.ambientOcclusion =      surfaceDescription.Occlusion;
		//                                        surfaceData.metallic =              surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		//                                normalTS =                   surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
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
		// based on HDPBRPass.template
		Name "Motion Vectors"
		Tags{ "LightMode" = "MotionVectors" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// If velocity pass (motion vectors) is enabled we tag the stencil so it don't perform CameraMotionVelocity
		Stencil
	{
		WriteMask 128
		Ref 128
		Comp Always
		Pass Replace
	}


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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_VELOCITY
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   FragInputs.positionRWS
		//   VaryingsMeshToPS.positionRWS
		//   AttributesMesh.positionOS

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
		//                                      #define ATTRIBUTES_NEED_NORMAL
		//                                      #define ATTRIBUTES_NEED_TANGENT
		//                                      #define ATTRIBUTES_NEED_TEXCOORD0
		//                                      #define ATTRIBUTES_NEED_TEXCOORD1
		//                                      #define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
		//                                      #define ATTRIBUTES_NEED_COLOR
#define VARYINGS_NEED_POSITION_WS
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
		float3 positionRWS; // optional
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.interp00.xyz = input.positionRWS;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.positionRWS = input.interp00.xyz;
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
		float3 TangentSpaceNormal; // optional
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

		output.positionRWS = input.positionRWS;
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		//                                        surfaceData.baseColor =             surfaceDescription.Albedo;
		//                                        surfaceData.perceptualSmoothness =  surfaceDescription.Smoothness;
		//                                        surfaceData.ambientOcclusion =      surfaceDescription.Occlusion;
		//                                        surfaceData.metallic =              surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		//                                normalTS =                   surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		//                                builtinData.emissiveColor =             surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassVelocity.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

		Pass
	{
		// based on HDPBRPass.template
		Name "Forward"
		Tags{ "LightMode" = "Forward" }

		//-------------------------------------------------------------------------------------
		// Render Modes (Blend, Cull, ZTest, Stencil, etc)
		//-------------------------------------------------------------------------------------
		Blend One Zero

		Cull Back

		ZTest LEqual

		ZWrite On

		// Stencil setup for forward
		Stencil
	{
		WriteMask 7
		Ref  2
		Comp Always
		Pass Replace
	}


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
		//                               #define _MATERIAL_FEATURE_SUBSURFACE_SCATTERING 1
		//                               #define _MATERIAL_FEATURE_TRANSMISSION 1
		//                               #define _MATERIAL_FEATURE_ANISOTROPY 1
		//                               #define _MATERIAL_FEATURE_CLEAR_COAT 1
		//                               #define _MATERIAL_FEATURE_IRIDESCENCE 1
		//                               #define _MATERIAL_FEATURE_SPECULAR_COLOR 1
		//                               #define _SURFACE_TYPE_TRANSPARENT 1
		//                               #define _BLENDMODE_ALPHA 1
		//                               #define _BLENDMODE_ADD 1
		//-------------------------------------------------------------------------------------
		// End Variant Definitions
		//-------------------------------------------------------------------------------------

#pragma vertex Vert
#pragma fragment Frag

#define UNITY_MATERIAL_LIT      // Need to be define before including Material.hlsl

		// Use surface gradient normal mapping as it handle correctly triplanar normal mapping and multiple UVSet
		// this modifies the normal calculation
		// #define SURFACE_GRADIENT

		// If we use subsurface scattering, enable output split lighting (for forward pass)
#if defined(_MATID_SSS) && !defined(_SURFACE_TYPE_TRANSPARENT)
#define OUTPUT_SPLIT_LIGHTING
#endif

#include "CoreRP/ShaderLibrary/Common.hlsl"
#include "CoreRP/ShaderLibrary/Wind.hlsl"

#include "CoreRP/ShaderLibrary/NormalSurfaceGradient.hlsl"

#include "ShaderGraphLibrary/Functions.hlsl"

		// define FragInputs structure
#include "HDRP/ShaderPass/FragInputs.hlsl"
#include "HDRP/ShaderPass/ShaderPass.cs.hlsl"

		//-------------------------------------------------------------------------------------
		// Defines
		//-------------------------------------------------------------------------------------
#define SHADERPASS SHADERPASS_FORWARD
#pragma multi_compile _ DEBUG_DISPLAY
#pragma multi_compile _ LIGHTMAP_ON
#pragma multi_compile _ DIRLIGHTMAP_COMBINED
#pragma multi_compile _ DYNAMICLIGHTMAP_ON
#pragma multi_compile _ SHADOWS_SHADOWMASK
#pragma multi_compile LIGHTLOOP_SINGLE_PASS LIGHTLOOP_TILE_PASS
#pragma multi_compile USE_FPTL_LIGHTLIST USE_CLUSTERED_LIGHTLIST
		// ACTIVE FIELDS:
		//   SurfaceDescriptionInputs.TangentSpaceNormal
		//   SurfaceDescription.Albedo
		//   SurfaceDescription.Normal
		//   SurfaceDescription.Metallic
		//   SurfaceDescription.Emission
		//   SurfaceDescription.Smoothness
		//   SurfaceDescription.Occlusion
		//   SurfaceDescription.Alpha
		//   SurfaceDescription.AlphaClipThreshold
		//   FragInputs.worldToTangent
		//   VaryingsMeshToPS.tangentWS
		//   VaryingsMeshToPS.normalWS
		//   AttributesMesh.tangentOS
		//   AttributesMesh.normalOS

		// this translates the new dependency tracker into the old preprocessor definitions for the existing HDRP shader code
#define ATTRIBUTES_NEED_NORMAL
#define ATTRIBUTES_NEED_TANGENT
		//                                      #define ATTRIBUTES_NEED_TEXCOORD0
		//                                      #define ATTRIBUTES_NEED_TEXCOORD1
		//                                      #define ATTRIBUTES_NEED_TEXCOORD2
		//                                      #define ATTRIBUTES_NEED_TEXCOORD3
		//                                      #define ATTRIBUTES_NEED_COLOR
		//                                      #define VARYINGS_NEED_POSITION_WS
#define VARYINGS_NEED_TANGENT_TO_WORLD
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
		float3 normalOS : NORMAL; // optional
		float4 tangentOS : TANGENT; // optional
	};
	struct VaryingsMeshToPS {
		float4 positionCS : SV_Position;
		float3 normalWS; // optional
		float4 tangentWS; // optional
	};
	struct VaryingsMeshToDS {
		float3 positionRWS;
		float3 normalWS;
	};
	struct PackedVaryingsMeshToPS {
		float3 interp00 : TEXCOORD0; // auto-packed
		float4 interp01 : TEXCOORD1; // auto-packed
		float4 positionCS : SV_Position; // unpacked
	};
	PackedVaryingsMeshToPS PackVaryingsMeshToPS(VaryingsMeshToPS input)
	{
		PackedVaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.interp00.xyz = input.normalWS;
		output.interp01.xyzw = input.tangentWS;
		return output;
	}
	VaryingsMeshToPS UnpackVaryingsMeshToPS(PackedVaryingsMeshToPS input)
	{
		VaryingsMeshToPS output;
		output.positionCS = input.positionCS;
		output.normalWS = input.interp00.xyz;
		output.tangentWS = input.interp01.xyzw;
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
		float3 TangentSpaceNormal; // optional
	};
	// Pixel Graph Outputs
	struct SurfaceDescription
	{
		float3 Albedo;
		float3 Normal;
		float Metallic;
		float3 Emission;
		float Smoothness;
		float Occlusion;
		float Alpha;
		float AlphaClipThreshold;
	};

	// Shared Graph Node Functions
	// Pixel Graph Evaluation
	SurfaceDescription SurfaceDescriptionFunction(SurfaceDescriptionInputs IN)
	{
		SurfaceDescription surface = (SurfaceDescription)0;
		surface.Albedo = IsGammaSpace() ? float3(0, 0.4333022, 1) : SRGBToLinear(float3(0, 0.4333022, 1));
		surface.Normal = IN.TangentSpaceNormal;
		surface.Metallic = 0;
		surface.Emission = IsGammaSpace() ? float3(0, 0, 0) : SRGBToLinear(float3(0, 0, 0));
		surface.Smoothness = 0.5;
		surface.Occlusion = 1;
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
		output.worldToTangent = BuildWorldToTangent(input.tangentWS, input.normalWS);
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
		output.TangentSpaceNormal = float3(0.0f, 0.0f, 1.0f);
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
		surfaceData.ambientOcclusion = 1.0f;
		surfaceData.subsurfaceMask = 1.0f;

		// copy across graph values, if defined
		surfaceData.baseColor = surfaceDescription.Albedo;
		surfaceData.perceptualSmoothness = surfaceDescription.Smoothness;
		surfaceData.ambientOcclusion = surfaceDescription.Occlusion;
		surfaceData.metallic = surfaceDescription.Metallic;
		//                                  surfaceData.thickness =             surfaceDescription.Thickness;
		//                                  surfaceData.diffusionProfile =      surfaceDescription.DiffusionProfile;
		//                                  surfaceData.subsurfaceMask =        surfaceDescription.SubsurfaceMask;
		//                                        surfaceData.specularColor =         surfaceDescription.Specular;

		// These static material feature allow compile time optimization
		surfaceData.materialFeatures = MATERIALFEATUREFLAGS_LIT_STANDARD;
#ifdef _MATERIAL_FEATURE_SUBSURFACE_SCATTERING
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SUBSURFACE_SCATTERING;
#endif
#ifdef _MATERIAL_FEATURE_TRANSMISSION
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_TRANSMISSION;
#endif
#ifdef _MATERIAL_FEATURE_ANISOTROPY
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_ANISOTROPY;
#endif
#ifdef _MATERIAL_FEATURE_CLEAR_COAT
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_CLEAR_COAT;
#endif
#ifdef _MATERIAL_FEATURE_IRIDESCENCE
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_IRIDESCENCE;
#endif
#ifdef _MATERIAL_FEATURE_SPECULAR_COLOR
		surfaceData.materialFeatures |= MATERIALFEATUREFLAGS_LIT_SPECULAR_COLOR;
#endif

		// tangent-space normal
		float3 normalTS = float3(0.0f, 0.0f, 1.0f);
		normalTS = surfaceDescription.Normal;

		// compute world space normal
		GetNormalWS(fragInputs, V, normalTS, surfaceData.normalWS);

		// TODO: use surfaceDescription tangent definition for anisotropy
		surfaceData.tangentWS = normalize(fragInputs.worldToTangent[0].xyz);    // The tangent is not normalize in worldToTangent for mikkt. TODO: Check if it expected that we normalize with Morten. Tag: SURFACE_GRADIENT
		surfaceData.tangentWS = Orthonormalize(surfaceData.tangentWS, surfaceData.normalWS);

		// Init other parameters
		surfaceData.anisotropy = 0;
		surfaceData.coatMask = 0.0f;
		surfaceData.iridescenceThickness = 0.0;
		surfaceData.iridescenceMask = 1.0;

		// Transparency parameters
		// Use thickness from SSS
		surfaceData.ior = 1.0;
		surfaceData.transmittanceColor = float3(1.0, 1.0, 1.0);
		surfaceData.atDistance = 1000000.0;
		surfaceData.transmittanceMask = 0.0;

		// By default we use the ambient occlusion with Tri-ace trick (apply outside) for specular occlusion.
		// If user provide bent normal then we process a better term
		surfaceData.specularOcclusion = 1.0;
#if defined(_BENTNORMALMAP) && defined(_ENABLESPECULAROCCLUSION)
		// If we have bent normal and ambient occlusion, process a specular occlusion
		surfaceData.specularOcclusion = GetSpecularOcclusionFromBentAO(V, bentNormalWS, surfaceData);
#elif defined(_MASKMAP)
		surfaceData.specularOcclusion = GetSpecularOcclusionFromAmbientOcclusion(NdotV, surfaceData.ambientOcclusion, PerceptualSmoothnessToRoughness(surfaceData.perceptualSmoothness));
#endif
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
		float3 bentNormalWS = surfaceData.normalWS;           // TODO : make bent normals work

		builtinData.opacity = surfaceDescription.Alpha;
		builtinData.bakeDiffuseLighting = SampleBakedGI(fragInputs.positionRWS, bentNormalWS, fragInputs.texCoord1, fragInputs.texCoord2);    // see GetBuiltinData()

																																			  // It is safe to call this function here as surfaceData have been filled
																																			  // We want to know if we must enable transmission on GI for SSS material, if the material have no SSS, this code will be remove by the compiler.
		BSDFData bsdfData = ConvertSurfaceDataToBSDFData(posInput.positionSS.xy, surfaceData);
		if (HasFlag(bsdfData.materialFeatures, MATERIALFEATUREFLAGS_LIT_TRANSMISSION))
		{
			// For now simply recall the function with inverted normal, the compiler should be able to optimize the lightmap case to not resample the directional lightmap
			// however it will not optimize the lightprobe case due to the proxy volume relying on dynamic if (we rely must get right of this dynamic if), not a problem for SH9, but a problem for proxy volume.
			// TODO: optimize more this code.
			// Add GI transmission contribution by resampling the GI for inverted vertex normal
			builtinData.bakeDiffuseLighting += SampleBakedGI(fragInputs.positionRWS, -fragInputs.worldToTangent[2], fragInputs.texCoord1, fragInputs.texCoord2) * bsdfData.transmittance;
		}

		builtinData.emissiveColor = surfaceDescription.Emission;
		builtinData.velocity = float2(0.0, 0.0);
#ifdef SHADOWS_SHADOWMASK
		float4 shadowMask = SampleShadowMask(fragInputs.positionRWS, fragInputs.texCoord1);
		builtinData.shadowMask0 = shadowMask.x;
		builtinData.shadowMask1 = shadowMask.y;
		builtinData.shadowMask2 = shadowMask.z;
		builtinData.shadowMask3 = shadowMask.w;
#else
		builtinData.shadowMask0 = 0.0;
		builtinData.shadowMask1 = 0.0;
		builtinData.shadowMask2 = 0.0;
		builtinData.shadowMask3 = 0.0;
#endif
		builtinData.distortion = float2(0.0, 0.0);           // surfaceDescription.Distortion -- if distortion pass
		builtinData.distortionBlur = 0.0;                        // surfaceDescription.DistortionBlur -- if distortion pass
		builtinData.depthOffset = 0.0;                        // ApplyPerPixelDisplacement(input, V, layerTexCoord, blendMasks); #ifdef _DEPTHOFFSET_ON : ApplyDepthOffsetPositionInput(V, depthOffset, GetWorldToHClipMatrix(), posInput);
	}

	//-------------------------------------------------------------------------------------
	// Pass Includes
	//-------------------------------------------------------------------------------------
#include "HDRP/ShaderPass/ShaderPassForward.hlsl"
	//-------------------------------------------------------------------------------------
	// End Pass Includes
	//-------------------------------------------------------------------------------------

	ENDHLSL
	}

	}
		FallBack "Hidden/InternalErrorShader"
}
