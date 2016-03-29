#t3

## Client

### Register

*Request*
POST /api/register
```json
{
  "name":"<user-name>"
}
```

*Response*
200
```json
{
  "Right": {
    "creds": {
      "name":"<user-name>",
      "key":"<user-key>",
    }
  }
}
```

### Trial match with 'random'

*Request*
POST /api/random
```json
{
  "creds": {
    "name":"<user-name>",
    "key":"<user-key>",
  }
}
```

*Response*
200
```json
{
  "state": {
    "final":null,
    "board": [
      [" "," "," "],
      [" "," "," "],
      [" "," "," "]
    ],
  }
  "users": {
    "x":"<user-name>",
    "o":"random"
  }
  "matchInfo": {
    "matchId":"<match-id>",
    "matchToken":"<x-match-token>",
  }
}
```
