In 1994 the The Eletronic Knights released their first [Trackmo called "Rampage" on AMIGA](https://www.youtube.com/watch?v=8UbVkbuxd1s).
I contributed the following parts and hereby release the source code under the ASL.


![Ellis](ellis/screen.png?raw=true)

While it looks trivial on todays computers drawing 6 six circles in real time on the AMIGA was a challenge. Even more so with clipping on edges. All dots in the circles had to be calculated and then be filled. In this scene they even get rotated and the colors are adjusted according the rotation.


![Mirror](mirror/screen.png?raw=true)

The mirror is rotating, so is the 3d object - a skewed cube in this case. The line drawing algorithm is highly optimized. There is the usual 3d rotation with fill but the most interesting part surely was to calculate the picture in the mirror by using optimized vector math. The light source calculation was the icing on the cake -that again- for todays eye doesn't really look that exciting anymore. But it was in 1994.
