# Configuration

```
{oauth2, [
  {backend, my_backend},
  {expiries, [
    {access_token, 2592000},
    {refresh_token, 2592000},
    {code_grant, 600}
  ]}
]}
```

