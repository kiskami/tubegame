cd dist
del * /Q

mkdir data
copy ..\COPYING .
copy ..\README.txt .
copy ..\..\data\level1.lisp data

mkdir data\backgrounds
copy ..\..\data\backgrounds\* data\backgrounds

mkdir data\fx
copy ..\..\data\fx\* data\fx

mkdir data\gui
mkdir data\gui\fonts
copy ..\..\data\gui\fonts\* data\gui\fonts

mkdir data\models
copy ..\..\data\models\*.mesh data\models
copy ..\..\data\models\*.material data\models

mkdir data\splash
copy ..\..\data\splash\* data\splash

mkdir bin
copy ..\..\target\OgreMain.dll bin
copy ..\..\target\Plugin_BSPSceneManager.dll bin
copy ..\..\target\Plugin_CgProgramManager.dll bin
copy ..\..\target\Plugin_OctreeSceneManager.dll bin
copy ..\..\target\Plugin_ParticleFX.dll bin
copy ..\..\target\RenderSystem_Direct3D9.dll bin
copy ..\..\target\RenderSystem_GL.dll bin
copy ..\..\target\cg.dll bin
copy ..\..\target\llgs-engine.dll bin
copy ..\..\target\plugins.cfg bin
copy ..\..\target\resources.cfg bin
copy ..\..\target\tubegame.exe bin

zip -r ..\tubegame-0.1.zip *

cd ..
