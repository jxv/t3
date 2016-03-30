#t3

## API

### Register

Request:
`POST /api/register`
```json
{
  "name":"<user-name>"
}
```

Response:
`200`
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

### Random bot

Request:
`POST /api/random`
```json
{
  "creds": {
    "name":"<user-name>",
    "key":"<user-key>",
  }
}
```

Response:
`200`
```json
{
  "state": {
    "final":null,
    "board": [
      [" "," "," "],
      [" "," "," "],
      [" "," "," "]
    ],
  },
  "users": {
    "x":"<user-name>",
    "o":"random"
  },
  "matchInfo": {
    "matchId":"<match-id>",
    "matchToken":"<match-token>",
  }
}
```

### Start

Request:
`POST /api/start`
```json
{
  "creds": {
    "name":"<user-name>",
    "key":"<user-key>",
  }
}
```

Response:
`200`
```json
{
  "state": {
    "final":null,
    "board": [
      [" "," "," "],
      [" "," "," "],
      [" "," "," "]
    ],
  },
  "users": {
    "x":"<user-name>",
    "o":"<opponent-user-name>"
  },
  "matchInfo": {
    "matchId":"<match-id>",
    "matchToken":"<match-token>",
  }
}
```

### Play

Request:
`POST /api/play/<match-id>/<match-token>`
```json
{
  "creds": {
    "name":"<user-name>",
    "key":"<user-key>",
  },
  "loc": {
    "x": 1,
    "y": 1
  }
}

```

Response:
`200`
```json
{
  "state": {
    "final":null,
    "board": [
      [" "," "," "],
      [" ","X"," "],
      [" "," "," "]
    ],
  }
}
```

#### Final Response

Response:
`200`
```json
{
  "state": {
    "final":"Won",
    "board": [
      ["X","O"," "],
      ["X","X","O"],
      ["O"," ","X"]
    ],
  }
}
```


