# t3

[![Build Status](https://travis-ci.org/jxv/t3.svg?branch=master)](https://travis-ci.org/jxv/t3)

## API

### Register

Request:
`POST /register`
```json
{
    "name": "<user-name>"
}
```

Response:
`200`
```json
{
    "creds": {
        "userId": "<user-id>",
        "token": "<user-token>"
    }
}
```

### Random bot (Not stored)

Request:
`POST /practice-lobby`
```json
{
    "creds": {
        "userId": "<user-id>",
        "token": "<user-token>"
    }
}
```

Response:
`200`
```json
{
    "start": {
        "gameId": "<game-id>",
        "o": "<user-id>",
        "x": "<user-id>"
    },
    "step": {
        "final": null,
        "board": [
            [" "," "," "],
            [" "," "," "],
            [" "," "," "]
        ]
    }
}

```

### Start

Request:
`POST /lobby`
```json
{
    "creds": {
        "userId": "<user-id>",
        "token": "<user-token>"
    }
}
```

Response:
`200`
```json
{
    "start": {
        "gameId": "<game-id>",
        "o": "<user-id-1>",
        "x": "<user-id-2>"
    },
    "step": {
        "final": null,
        "board": [
            [" "," "," "],
            [" "," "," "],
            [" "," "," "]
        ]
    }
}
```

### Play

Request:
`POST /play`
```json
{
    "creds": {
        "userId": "<user-id>",
        "token": "<user-token>"
    },
    "gameId": "<game-id>",
    "loc": {
        "x": 1,
        "y": 1
    }
}

```

Response (Unfinished):
`200`
```json
{
    "step": {
       "final": null,
       "board": [
           [" "," "," "],
           [" ","X"," "],
           [" "," "," "]
      ],
    }
}
```

Response (Finished):
`200`
```json
{
    "step": {
        "final":"Won",
        "board": [
            ["X","O"," "],
            ["X","X","O"],
            ["O"," ","X"]
        ]
    }
}
```

### Game result(s)

Request:
`POST /result`

```json
{
    "gameId": "<game-id>"
}
```

Response:
`200`
```json
{
    "result": {
        "start": {
            "gameId": "<game-id>",
            "o": "<user-id-1>",
            "x": "<user-id-2>"
        },
        "board": [
            ["X"," "," "],
            ["O","X"," "],
            [" ","O","X"]
        ],
        "loser": "O",
        "winner": "X"
    }
}
```
