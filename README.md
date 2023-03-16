## echo-bot

# init project

To initialize the working environment you can use the lint in the scripts folder or create the folders you need to work correctly yourself. 

If you want to create the correct configuration of folders and files, you will have to:
1. Create a logs folder.
2. Move to the folder you created.
3. Create a log.text file.
4. Create a responseBody.json file.

Folder with an example configuration file has already been created and briefly described. But here I will also give you what the fields mean.
* confConfigurationTypes - TelegramFrontEnd or ConsoleFrontEnd.
* confHelpReply - text for help reply.
* confRepeatReply - text for repeat reply.
* confRepetitionCount - one to five.
* preconfFilePath - path to logs.
* preconfMinLevel - log level: Debug, Info, Warning, Error.
* confBotToken - A token from your bot.

# Structure

The project structure is divided into interface, implementation, configuration.

Top of source directory: handler interface, types, general configuration.

In child directory nodes: implementation.

In more detail:
* At the top are the files: ConfigBot, ConfigurationTypes, EchoBot, Logger.
* The folder Logger contains the implementation logger.
* The folder FrontEnd contains the implementation consol fronend and telegram frontend.
* The console implementation is a file with the same name, unlike the telegram implementation, which consists of sub-catalogs.
* On top of the Telegram folder are the files: API.hs - implements the api through the servant, Telegram.hs - frontend implementation.
* The Date folder contains the structures used in the api: GetUpadate.hs - The main structure for getting updates for messages and polls; PollMessage.hs, SendMessage.hs, SendPhoto.hs - structures received when sending messages, photos, polls.
