/*  Settings specific for debugging session

    This is central file, if you need to create 
    user/machine specific settings then  use file `user.settings.pl`. 

    Production specific settings can be provided in 'app.settings.pl', 
    but preferred way is to use environnment variables in the production deployments.
*/


%	HTTP port the server is listening on. (ENV http_port)
setting(server:port, 8280).
