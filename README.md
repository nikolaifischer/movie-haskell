# Watch That! (movie-haskell)

![Screenshot](https://raw.githubusercontent.com/nikolaifischer/movie-haskell/master/static/img/monitor_tablet_mockup.jpg)

### What is "Watch That"?
"Watch That!" is a platform designed to help you keep track of your movie recommendations.
Don't you have that one friend which always recommends movies that you might like, but you never get around to watching them? "Watch That!" helps you remember which friend recommended which movie to you.

### How does it work?
To recommend a movie to a friend all you have to know is his or her E-Mail Address. You can search for any movie and than send a recommendation to your friend.
The movies your friends recommend to you are displayed in your account. Anytime you want to enjoy a good movie but don't know which one, you can open up "Watch That!" and get inspired by your friends' recommendations.

### Technology Stack
"Watch That!" is written in Haskell using the Yesod Framework. As a Database it uses MongoDB.

### Install
* Download or clone the project
* Start a MongoDB Server on your Machine on Port 27017.
    * Note: DB Settings can be overwritten using ENV-Variables:
    * DATABASE_HOST
    * DATABASE_PORT
    * DATABASE_PASSWORD
    * DATABASE_USER
    * DATABASE_NAME
* In a Command Line run:
    * stack setup
    * stack install --only-dependencies
    * stack install
    * Run the built movie-haskell.exe located in the .stack-work folder
    * OR
    * run stack exec -- yesod devel    for a development build
