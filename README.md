# Haskbot

### An easily-extensible, Haskell-based Slack chatbot

The purpose of this little bot is to provide:

- a slew of mini-services for [Bendyworks](http://bendyworks.com/)
- a simple platform for Bendyworkers to learn Haskell
- a playground for exciting Haskell web modules, such as WAI, Warp, and Aeson

This `README` only demonstrates how to setup a Haskell dev environment for
creating Haskbot plugins. Further documentation can be found on
[Hackage](http://hackage.haskell.org/package/haskbot-core).

### New to Haskell?

I find enjoyment of Haskell is greatly increased by a thorough reading of the
first few chapters of [Learn You a Haskell...](http://learnyouahaskell.com). Of
course, I highly recommend the entire book when you've time.

If you're an in-house Bendyworker, I'm also available before/after hours or
during growth time to help or answer any questions.

### Installing the Haskell Platform and Haskbot

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
   - On OSX (via [Homebrew](http://brew.sh/))

     ```sh
     brew update
     brew install haskell-platform
     ```

2. Add Cabal's (the Haskell package manager) `bin` folder to your shell's
   `$PATH`. This is usually done by adding the following lines to
   `~/.profile` or `~/.bash_profile` (whichever you have/prefer).
   ```sh
   PATH="$HOME/.cabal/bin:$PATH"
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
   cabal install haskbot-core
   ```

### Creating Plugins

You're now ready to begin creating plugins for your very own Haskbot! Continue
on to [Hackage](http://hackage.haskell.org/package/haskbot-core) for a full
Haskbot API description and examples.

### Thanks

I wouldn't have had time to write this without the growth time supplied by
[Bendyworks](http://bendyworks.com/). Hey, employers! This is what developers
need to survive.

### License

See `LICENSE.txt`
