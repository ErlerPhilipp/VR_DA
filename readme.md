Basketball
==========

![Basketball screen shot](Basketball.png "Basketball screen shot")

The Basketball project is a virtual reality game for the HTC Vive. You can grab a ball and throw it through a hoop. The aim is to get as many points as possible in the given time. There are multiple rounds with increasing difficulty.

How to Start
------------

 1. Start SteamVR and make sure all controllers are detected.
 2. Start Basketball.exe.
 3. Put on the head mounted display, controller loops and earphones.

How to Play
------------

 * Pull the trigger (at your pointing finger) when the controller is near the ball to grab it.
 * Release the trigger to throw the ball.
 * There are several rounds:
  1. The warm-up phase is free time to play around. The game starts after you scored 3 times.
  2. The hoop does not move. Each of the following rounds gives you 60 seconds to maximize your score. 
  3. The hoop jumps after each hit.
  4. The hoop moves from left to right and back.
  5. The hoop moves in a circle.
  6. The hoop moves in a circle twice as fast. Afterwards, your final score is displayed and you're back in the warm-up.

Grabbing Test
==========

![GrabbingTest screen shot](GrabbingTest.png "GrabbingTest screen shot")

The grabbing test is a virtual reality game for the HTC Vive. You get a point every time you throw the wanted ball through the hoop. The wanted ball, 1 out of 4, is displayed at the hoop. The aim is to get as many points as possible in the given time. There are multiple rounds with increasing difficulty.

How to Start
------------

 1. Start SteamVR and make sure all controllers are detected.
 2. Start GrabbingTest.exe directly or with command line parameter for certain types of feedback. There are .bat files in the solution directory for every combination of feedbacks.
 3. Put on the head mounted display, controller loops and earphones.

How to Play
------------

 * Pull the trigger (at your pointing finger) when the ball is within the selection volume to grab it. If enabled, haptic feedback (vibration) and optical feedback (highlight) show you if the ball can be grabbed.
 * Release the trigger to throw the ball.
 * The ball at the hoop shows which ball is required to score.
 * There are several rounds. The selection volume shrinks after each round approaching the controller's collidier.
 * The light is yellow during the warm-up phase. Also, there is a short green flash when a new round begins.

Notes
==========

How to Build
------------

 1. Clone the repository or download the zip.
 2. Open a command line window in the solution directory.
 3. Execute "build install". The build script will now download and install the dependencies.
 4. Open the solution in Visual Studio (2015).
 5. Set the desired project as start-up project.
 6. Build and run the application. Better start without debugger and attach later. Otherwise, the shader compilation can take up to 5 minutes.

Credits
------------
 * We use textures from http://www.textures.com/. The textures are converted to compressed formats.
  * http://www.textures.com/download/3dscans0045/127218
  * http://www.textures.com/download/substance0027/127027
  * http://www.textures.com/download/3dscans0029/126909
  * http://www.textures.com/download/3dscans0024/126738
  * http://www.textures.com/download/substance0021/126935
  * http://www.textures.com/download/substance0003/125731
 * Ball textures:
  * http://opengameart.org/content/basket-ball-texture
  * http://www.robinwood.com/Catalog/FreeStuff/Textures/TexturePages/BallMaps.html
 * Hoop model and textures:
  * http://tf3dm.com/3d-model/who-love-basketball--18784.html
 * Sound effects exported as wav with Microsoft encoding
  * Bounce (cut): https://www.freesound.org/people/Juan_Merie_Venter/sounds/327687/
  * Ambient 1: https://www.freesound.org/people/goulven/sounds/371277/
  * Ambient 2: https://www.freesound.org/people/PatrickLieberkind/sounds/245187/
  * Pop sound: https://www.freesound.org/people/qubodup/sounds/222373/
  * Sirene: https://www.freesound.org/people/santino_c/sounds/170825/
