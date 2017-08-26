# Aug 21 2017 - 4:00 - closed - Rendering capabilities in the platform layer
* debug system
In order to properly time functions, the entire debug system needed to exist
outside of the game code (specifically, any timed functions).  The only place
where it could live is the platform layer, however it also needs to draw to the
screen, so the rendering code had to be lofted up into the platform layer.  3h
of refactoring and a while figuring out what was going wrong.

# Aug 13 2017 - 0:30 - closed - Actually use the new Destroyed state
* entity state
* lifecycle

# Aug 13 2017 - 0:30 - closed - Put player into entity table
* player
* entity system
* collision detection
Player update and the rest of entity update code was not exactly the same, thus
there was a chance for entities to tunnel through the Player

# Aug 13 2017 - 0:45 - closed - Fix PushFrameEvent
* frame queue
Had some bad logic to step through a linked list I clearly didn't test

# Aug 10 2017 - 10:00 - closed - Fix shadow mapping
* opengl
* shadow mapping
Epic quest to fix the shadow mapping turned out to be mostly a wild goose chase
and a couple hours tuning it at the end.  The wild goose chase part was trying
to translate the world geometry using a transform matrix - which I still think
should have worked - instead of repositioning the projection matrix with an
offset.  The other part was changing the shader to use sampler2d instead of
sampler2dShadow, which I'm not sure was a good idea, but I felt like doing it.

# Aug 1 2017 - 0:15 - closed - Systems sometimes unspawning early
* particle system
Parent entities sometimes got to the end of the world before the whole effect
had time to complete, causing it to disappear in a jarring fashon.

# Aug 1 2017 - 1:00 - closed - Inconsistent physics update.
* physics
* frame rate
Introduced during refactor with variable frame rate

# Jul 30 2017 - 1:00 - closed - Globals causing problems; moved into game_state
* globals
* hot reload

# Jul 30 2017 - 0:45 - closed - GameThreadCallback not refreshed on reload
* lifecycle
* hot reload

# Jul 24 2017 - 3:00 - closed - Some entries getting skipped
* thread queue
Problem introduced going from checking the thread queue for work -> semaphore
only for flow control.  Whenever AtomicCompareExchange failed, ThreadSleep
would be called again before ACE was retried, thus leaving the Semaphore out of
sync with the number of entries on the queue.

# Jul 24 2017 - 0:25 - closed - buffer overrun in frame queue
* frame queue

# Jul 24 2017 - 0:05 - closed - Reinitialization error
* particle system

# Jul 24 2017 - 0:05 - closed - frame rate higher than normal; no vsync
* particle system
* frame rate

