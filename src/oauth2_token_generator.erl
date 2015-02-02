-module(oauth2_token_generator).

-callback generate(oauth2:context()) -> oauth2:token().
