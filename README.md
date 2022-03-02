Add spago bash completion:
```shell
source <(spago --bash-completion-script $(which spago))
```

Automatic rebuild:
```shell
spago build --watch
```

Start Json echo server with:
```shell
node src/echo-server.js
```

For testing the local Json server you can use curl, e.g.
```shell
curl -X POST http://localhost:8000/ -d '{"productId": 123456, "quantity": 100}'
```
