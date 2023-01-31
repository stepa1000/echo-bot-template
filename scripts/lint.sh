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
cat <<EOF>global.yaml
confConfigurationTypes: ConsoleFrontEnd
confEchoBot:
  confHelpReply: text for HelpReply
  confRepeatReply: text for RepeatReply
  confRepetitionCount: 3
confLogger:
  preconfFilePath: ./logs/log.text
  preconfMinLevel: Debug
confTelegram:
  confBotToken: ''
EOF
cd ..
stack test
stack build
hlint .
