# Haskbot

### A Haskell-based chatbot using the Scotty web framework.

The purpose of this little bot is to provide:

- a slew of mini-services for [Bendyworks](http://bendyworks.com/)
- a simple platform for Bendyworkers to learn Haskell
- a playground for exciting Haskell web modules, such as WAI, Warp, and Aeson

Because this application is designed for beginners, setup will begin with
bootstrapping a Haskell development environment. If you already know the
basics, feel free to skip to [bootstrapping](#bootstrapping) or
[creating-a-plugin](#creating-a-plugin).

### New to Haskell?

I find enjoyment of Haskell is greatly increased by a thorough reading of the
first few chapters of [Learn You a Haskell...](http://learnyouahaskell.com). Of
course, I highly recommend the entire book when you've time.

If you're an in-house Bendyworker, I'm also available before/after hours or
during growth time to help or answer any questions.

## Bootstrapping

### Installing the Haskell Platform

To run Haskbot locally, all you require is the latest Haskell platform. If your
distro of choice isn't
[Debian](http://www.extellisys.com/articles/haskell-on-debian-wheezy),
this means your package manager probably provides it for you.

1. Run the following to install the platform:
   - On Ubuntu
     ```sh
     sudo apt-get update
     sudo apt-get install haskell-platform
     ```
   - On OXS
     ```sh
     brew update
     brew install haskell-platform
     ```
2. Add Cabal's (the Haskell package manager) `bin` folder to your shell's
   `$PATH`. This is usually done by adding the following lines to
   `~/.profile` or `~/.bash_profile` (whichever you have/prefer).
   ```sh
   PATH="/home/[your_username]/.cabal/bin:$PATH"
   export PATH
   ```
   Make sure to re-source whatever file contains the new `$PATH`, like so:
   ```sh
   source ~/.profile
   ```
3. Update the Haskell packages via:
   ```sh
   cabal update
   cabal install cabal-install
   ```

### Bootstrapping a Haskbot development environment

Once you have the platform, setting up a development environment is a breeze
(but it does take a few minutes while everything compiles).

1. Clone this repository into the directory of your choice via:
   ```sh
   git clone https://github.com/Jonplussed/Haskbot.git
   ```
2. Install any required dependencies via:
   ```sh
   cd [your Haskbot directory]
   cabal sandbox init
   cabal install --only-dependencies
   ```
3. Build, baby, build!
   ```sh
   cabal build
   ```
4. Run the server with the required environmental variables:
   ```sh
   SLACK_TOKEN=[you'll need to get this] \
   PORT=[your port of choice] \
   ./dist/build/Haskbot/Haskbot
   ```
8. Test that the Haskbot server is running by pinging
   `http://localhost:[your port from above]`.

### Creating Plugins

Plugins for Haskbot are simply functions that parse an input string and return
an output string (and perhaps do something side-effecty on the server).
Adding a plugin for Haskbot is simple process:

1. **Add the requisite files**

   You only need to create two files to construct a complete plugin:
   - `src/Plugins/[YourPluginName].hs` holds your plugin code
   - `test/Plugins/[YourPluginName]Spec.hs` holds your plugin's tests

2. **Write tests for your plugin**

   Plugins tests are written in the fantastic [HSpec](http://hspec.github.io/)
   testing DSL. Because most plugins are a simple input string to output
   string transformation, the most common test case has been simplified
   with the `responsesFor` spec helper.
   ```haskell
   spec :: Spec
   spec = do
     describe "myPluginName" $ do
       responsesFor myPlugin
         [ ("first input string",   "first expected output string")
         , ("second input string",  "second expected output string")
         ]
   ```
  Of course, any plugins with side effects (such as setting a Redis key/value)
  will require additional, more complicated specs.

  To run specs, from your project root, run:
  ```sh
  cabal build spec && ./dist/build/spec/spec
  ```
  to rebuild the project and see the HSpec output.

3. **Write your plugin**

   Plugins are simply functions that parse an input string, maybe do something
   side-effecty, and return an output string. `Text.Parsec` is directly
   available for the more experienced Haskellers; for the newer, some
   prefabricated parsers can be found in the `Parser.Commons` module.

   **Example:** A plugin that takes the first word as a command and the rest of
   the text as the only argument can then be written as:
   ```haskell
   myPluginName :: Plugin
   myPluginName = commandWithText "trigger" someOutput

   someOutput :: String -> String
   someOutput = ... -- function body goes here
   ```

4. **Register your plugin**

   To include your plugin in those run by Haskbot, import your plugin module in
   the `Registry` module and add your plugin function to the list in the
   `pluginsFor` function.

5. **Send a pull request**

   I'll take care of compiling and deploying the updated Haskbot. Send me a
   me a pull request, I'll ensure your plugin tested, and I'll redeploy
   Haskbot with new functionality included!

### More

Haskbot currently supports only [Slack](https://api.slack.com/), but
additional protocols can be added in the `src/Protocols/` directory. Each
protocol handles the request/response cycle differently, so nothing is
currently abstracted. See the `Protocols.Slack...` modules for one example of
a simple request/response handler.

### Thanks

I wouldn't have had time to write this without the growth time supplied by
[Bendyworks](http://bendyworks.com/). Hey, employers! This is what developers
need to survive.

This project couldn't be hosted on Heroku without Joe Nelson's amazing
[Heroku buildpack](https://github.com/begriffs/heroku-buildpack-ghc). Holy
crap, what a script.

### License

See `LICENSE.txt`
