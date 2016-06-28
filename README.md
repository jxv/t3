#t3

[![Build Status](https://travis-ci.org/jxv/t3.svg?branch=master)](https://travis-ci.org/jxv/t3)


## Client docs

http://hackage.haskell.org/package/t3-client

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
  "creds": {
    "name":"<user-name>",
    "key":"<user-key>",
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

### Match Playbacks

Request:
`GET /api/match/<match-id>`

Response:
`200`
```json
{
  "users": {
    "o":"<user-name-1>",
    "x":"<user-name-2>"
  },
  "actions": [
    {"xO":"X", "loc": {"x":0, "y":0}},
    {"xO":"O", "loc": {"x":1, "y":1}},
    {"xO":"X", "loc": {"x":2, "y":2}},
    {"xO":"O", "loc": {"x":1, "y":0}},
    {"xO":"X", "loc": {"x":2, "y":1}},
    {"xO":"O", "loc": {"x":1, "y":2}}
  ],
  "result": {
    "tag":"decision",
    "winner":"O",
    "loser":"X"
  },
  "matchId":"<match-id>"
}
```
