{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts, GADTs #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

import        LambdaCube.GL
import        LambdaCube.GL.Mesh

import FRP.Elerea.Simple

import Common.Utils hiding (risingEdge)
import Common.GraphicsUtils

import Codec.Image.STB hiding (Image)
import FX

texturing :: Exp Obj (Texture Tex2D SingleTex (Regular Float) RGBA) -> Exp Obj (VertexStream Triangle (V3F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing tex objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    --rasterCtx = TriangleCtx (CullNone) PolygonFill NoOffset LastVertex
    rasterCtx = TriangleCtx (CullBack CW) (PolygonLine 1) NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT

    emptyFB :: Exp Obj (FrameBuffer 1 (Float,V4F))
    emptyFB = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 0.4 1):.ZT)

    --fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize rasterCtx primitiveStream

    --primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    modelViewProj :: Exp V M44F
    modelViewProj = Uni (IM44F "MVP")

    --vertexShader :: Exp V (V3F,V2F) -> VertexOut () V2F
    vertexShader puv = VertexOut v4 (Const 1) ZT ZT
      where
        v4 :: Exp V V4F
        v4 = modelViewProj @*. v3v4 p
        p = puv
        --(p,uv) = untup2 puv

    --fragmentShader :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader uv = FragmentOutRastDepth $ Const one' :. ZT

color t uv = texture' (smp t) uv
smp t = Sampler LinearFilter ClampToEdge t

risingEdge :: Signal Bool -> SignalGen (Signal Bool)
risingEdge signal = do
    signal' <- delay True signal
    memo $ liftA2 (&&) signal (not <$> signal') 

main :: IO ()
main = do
    (win,windowSize) <- initWindow "LambdaCube 3D Textured Cube" 800 600
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

    let texture = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1
        frameImage :: Exp Obj (Image 1 V4F)
        frameImage = PrjFrameBuffer "" tix0 $ texturing texture (Fetch "stream" Triangles (IV3F "position"{- , IV2F "vertexUV" -}))

        --fx img = PrjFrameBuffer "" tix0 $ texturing (imgToTex $ postProcess $ img) (Fetch "stream" Triangles (IV3F "vertexPosition_modelspace", IV2F "vertexUV"))
        imgToTex img = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]

    renderer <- compileRenderer $ ScreenOut $ frameImage
    initUtility renderer

    let uniformMap      = uniformSetter renderer
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap
        setWindowSize   = setScreenSize renderer

    setWindowSize 800 600
    Right img <- loadImage "hello.png" -- "uvtemplate.bmp"
    texture =<< compileTexture2DRGBAF True False img


    let scale = 0.3
        virus = do
          gpuCube <- compileMesh $ sphere (0.1*scale) 2
          o1 <- addMesh renderer "stream" gpuCube []

          gpuCube <- compileMesh $ sphere (0.3*scale) 3
          o2 <- addMesh renderer "stream" gpuCube []

          gpuCube <- compileMesh $ sphere (0.6*scale) 4
          o3 <- addMesh renderer "stream" gpuCube []

          gpuCube <- compileMesh $ sphere (0.9*scale) 5
          o4 <- addMesh renderer "stream" gpuCube []

          gpuCube <- compileMesh $ sphere (1.2*scale) 100
          --o5 <- addMesh renderer "stream" gpuCube []
          --return $ [o1,o2,o3,o4,o5]
          return ()
    virus
    gpuCube4 <- compileMesh $ sphere (0.6*scale) 4
    {-
      fire from a position
      
    -}
    setTime 0
    Just t <- getTime
    
    (angleMove,setAngleMove) <- external 0
    (shoot,setShoot) <- external False
    (absTime,setAbsTime) <- external t
    net <- start $ do
      deltaTime <- (fmap fst) <$> transfer (0,0) (\a (_,b) -> (a - b,a)) absTime
      shootNow <- risingEdge shoot
      angle <- transfer2 0 (\t a b -> b + t * a) deltaTime angleMove
      let stepBullet t ((px,py),d@(dx,dy),s) = ((px+dx*t,py+dy*t),d,s)
          newBullet a s = if not s then return [] else do
            bulletOb <- addMesh renderer "stream" gpuCube4 ["MVP"]
            return [(pos0,dir a,bulletOb)]
          dir a = (cos a,sin a)
          pos0 = (0,0)
      newBullet' <- effectful2 newBullet angle shootNow
      bullets <- transfer2 [] (\t new l -> new ++ map (stepBullet t) l) deltaTime newBullet'
      let mainSignal = do
            (,) <$> angle <*> bullets
      return mainSignal

    --let cm  = fromProjective (lookat (Vec3 4 0.5 (-0.6)) (Vec3 0 0 0) (Vec3 0 1 0))
    let cm  = fromProjective (lookat (Vec3 3 1.3 0.3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (800 / 600)
        setInput = do
            Just t <- getTime
            setAbsTime t
            space <- keyIsPressed Key'Space
            left <- keyIsPressed Key'Left
            right <- keyIsPressed Key'Right
            let moveSpeed = 100
            setAngleMove $ if left then -moveSpeed else if right then moveSpeed else 0
            setShoot space

        loop = do
            setInput
            (t,bl) <- net
            --print bl
            let angle = pi / 24 * realToFrac t
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
            mvp $! mat4ToM44F $! mm .*. cm .*. pm
            forM_ bl $ \((x,y),_,o) -> do
              let objUniMap = objectUniformSetter o
                  mvpObj = uniformM44F "MVP" objUniMap
                  mm = fromProjective $ translation (Vec3 (realToFrac x) (realToFrac y) 0)
              mvpObj $! mat4ToM44F $! mm .*. cm .*. pm
              return ()
            render renderer
            swapBuffers win >> pollEvents

            k <- keyIsPressed Key'Escape
            unless k $ loop
    loop

    dispose renderer
    destroyWindow win
    terminate

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)
