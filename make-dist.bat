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

mkdir bin
copy ..\..\target\OgreMain_d.dll bin
copy ..\..\target\Plugin_BSPSceneManager_d.dll bin
copy ..\..\target\Plugin_CgProgramManager_d.dll bin
copy ..\..\target\Plugin_OctreeSceneManager_d.dll bin
copy ..\..\target\Plugin_ParticleFX_d.dll bin
copy ..\..\target\RenderSystem_Direct3D9_d.dll bin
copy ..\..\target\RenderSystem_GL_d.dll bin
copy ..\..\target\cg.dll bin
copy ..\..\target\llgs-engine-debug.dll bin
copy ..\..\target\plugins_d.cfg bin
copy ..\..\target\resources_d.cfg bin
copy ..\..\target\tubegame.exe bin

zip -r ..\tubegame-0.1.zip *

cd ..
