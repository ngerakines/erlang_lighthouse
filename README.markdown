
erlang\_lighthouse is an Erlang client library to interface with the lighthouseapp.com API.

**This is ALPHA software and is actively being developed and changed. Do Not Use.**

## Features

 * Can support multiple forms of authentication including username/password combinations as well as API keys.

## Use

The first two parameters of every API function will always be the server and the authentication information. The server variable is the base name of the lighthouse domain.

For the activereload.lighthouseapp.com domain, the server variable would be "activereload".

The second parameter is the authentication information. This is either a 1 or 2 value tuple. The one value tuple's single value is a string representing a valid API token.

The two value tuple is used for username and password authentication

### Example

    $ erl
    ...
    1 > lighthouse:projects("activereload", {"APIKEY123"}).
    {projects,[{type,"array"}],
              [{project,[],
                        [{archived,[{type,"boolean"}],["false"]},
                         {'created-at',[{type,"datetime"}], ... }
    2> lighthouse:projects("activereload", {"username", "password"}).
    {projects,[{type,"array"}],
              [{project,[],
                        [{archived,[{type,"boolean"}],["false"]},
                         {'created-at',[{type,"datetime"}], ... }

## Support

**Note**: This library requires r12-5 or later.

Please use http://socklabs.lighthouseapp.com/projects/23169-erlang_lighthouse/overview for any issues or questions for this library.
