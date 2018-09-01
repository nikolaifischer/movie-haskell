# Watch That! (movie-haskell)

![Screenshot](https://raw.githubusercontent.com/nikolaifischer/movie-haskell/master/static/img/monitor_tablet_mockup.jpg)

### What is "Watch That"?
"Watch That!" is a platform designed to help you keep track of your movie recommendations.
Don't you have that one friend which always recommends movies that you might like, but you never get around to watching them? "Watch That!" helps you remember which friend recommended which movie to you.

I made this project open source to help other programmers in getting their haskell web apps running. You are welcome to use my code for non-commercial purposes!

### How does it work?
To recommend a movie to a friend all you have to know is his or her E-Mail Address. You can search for any movie and than send a recommendation to your friend.
The movies your friends recommend to you are displayed in your account. Anytime you want to enjoy a good movie but don't know which one, you can open up "Watch That!" and get inspired by your friends' recommendations.

### Technology Stack
"Watch That!" is written in Haskell using the Yesod Framework. As a Database the App uses MongoDB.

### Screenshots
More screenshots of the app can be found here: [Imgur](https://imgur.com/a/fOXbEAm)
### Install
* Prerequisites:
  * [MongoDB](https://www.mongodb.com/)
  * [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
* Download or clone the project
* Create and enter a Google API key:
  * Visit https://console.cloud.google.com
  * Add a new Project and select it
  * Search for the Google+ API and enable it
  * Click on API & Services -> Credentials
  * Click Create Credentials
  * Pick "OAuth Client ID"
  * Pick "Web Application"
  * Enter an arbitray name
  * Add http://localhost:3000 to authorised JavaScript origins
  * Add http://localhost:3000/auth/page/googleemail2/complete to authorised redirect URIs
  * Open the file Foundation.hs of this project
  * Input the Client ID in the "clientID" variable and the client secret in the "clientSecret" variable

* Create an The Movie DB API key:
  * Visit https://www.themoviedb.org
  * Create a free account
  * Open your account page and click on "Settings"
  * Select "API" and follow the instructions to create an API key
  * Copy your API key and paste it in the file Foundation.hs in the variable called "key"

* Start a MongoDB Server on your Machine on Port 27017.
    * Note: DB Settings can be overwritten using Environment-Variables:
    * DATABASE_HOST
    * DATABASE_PORT
    * DATABASE_PASSWORD
    * DATABASE_USER
    * DATABASE_NAME
* In a Command Line run:
    * stack setup
    * stack install --only-dependencies
    * stack install
    * Start the app:
      * Copy the built movie-haskell.exe located in the .stack-work/install/.../bin folder to the root of the project
      * Run movie-haskell.exe as root/administrator
    * OR Start in Development Mode
      * run stack exec -- yesod devel (yesod command line tool is needed)
