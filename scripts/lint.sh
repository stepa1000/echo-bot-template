#!/bin/sh

set -eu

cd "$(git rev-parse --show-toplevel)"
# find . -name '*.hs' -print0 | xargs -0n100 ormolu --mode inplace
mkdir config
mkdir logs
cd logs
cat <<EOF>log.text
EOF
cat <<EOF>responseBody.json
EOF
cd ..
cd config
cat <<EOF>ConfigurationTypes.yaml
configuration: "Console"
EOF
cat <<EOF>EchoBot.yaml
echobot:
  confHelpReply: "text for help reply"
  confRepeatReply: "text for repeat reply"
  confRepetitionCount: 3
EOF
cat <<EOF>LoggerImp.yaml
loggerimp:
  confFileHendler: "./logs/log.text"
  confMinLevel: "Debug"
EOF
cat <<EOF>Telegram.yaml
tokenbot: "your token"
namebot: "your bot name"
EOF
cd ..
stack test
stack build
hlint .
