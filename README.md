Please run the following check:

```
$ sbt compile test run
```

The output should look like this:

```
[info] welcome to sbt 1.5.4 (Debian Java 17-ea)
[info] loading project definition from /home/minoru/src/icfpc-2021/project
[info] loading settings for project icfpc-2021 from build.sbt ...
[info] set current project to icfpc-2021 (in build file:/home/minoru/src/icfpc-2021/)
[info] Executing in batch mode. For better performance use sbt's shell
[success] Total time: 0 s, completed 3 Jul 2021, 20:07:34
[info] ExampleSpec:
[info] 2+2
[info] - should be equal to 4
[info] Run completed in 250 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 1 s, completed 3 Jul 2021, 20:07:35
[info] running Main
Hello, world!
[success] Total time: 0 s, completed 3 Jul 2021, 20:07:35
```

If everything's fine, add your name to the list below (so we know you got it
working, and that you can push to this repo):

- Minoru
