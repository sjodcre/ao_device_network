# 🔗 ao_mesh – Mini Node-to-Node Messaging System in Erlang

This project simulates a distributed messaging system inspired by Hyperbeam/AO Core. Each Erlang node hosts devices (like microservices) and can send structured messages to other nodes for routing and processing.

## 📦 Folder Structure

```
ao_mesh/
├── src/
│   ├── ao_node.erl           # Message router (gen_server)
│   ├── device_logger.erl     # Example device that logs messages
│   ├── device_math.erl       # Example device that does math
│   ├── device_registry.erl   # Key-to-module dynamic dispatcher
│   └── ao_mesh.erl           # (Optional app entry point)
├── rebar.config
├── README.md
```

## 🚀 Getting Started

### ✅ 1. Compile
```bash
rebar3 compile
```

### ✅ 2. Run nodes in separate terminals

#### Terminal 1 – node1
```bash
erl -sname node1 -setcookie beamcookie -pa _build/default/lib/ao_mesh/ebin
```

```erlang
ao_node:start_link().
```

#### Terminal 2 – node2
```bash
erl -sname node2 -setcookie beamcookie -pa _build/default/lib/ao_mesh/ebin
```

```erlang
ao_node:start_link().
```

## 💡 Dynamic Registration

In the target node (e.g. node2):

### Register a single command to a module:
```erlang
device_registry:register(<<"ping">>, device_logger).
```

### Register all keys from a device module:
```erlang
lists:foreach(fun(Key) -> device_registry:register(Key, device_math) end, device_math:keys()).
```

## 🧪 Testing

From node1 shell:

```erlang
ao_node:send(node2, "log Hello World").
ao_node:send(node2, "ping are you alive?").
ao_node:send(node2, "add 5 7").
ao_node:send(node2, "mul 2 3 4").
```

Expected:
```erlang
[ao_node] Reply: {ok,<<"logged">>}
[ao_node] Reply: {ok,<<"pong">>}
[ao_node] Reply: {ok,12}
[ao_node] Reply: {ok,24}
```

## 🧠 Concepts Covered

- Distributed Erlang nodes
- Message passing across nodes
- Dynamic device dispatch with ETS
- Device registration/unregistration at runtime
- Plug-and-play message handlers (like AO microservices)

## 🧹 Extra Commands

### Check what's registered:
```erlang
device_registry:list().
```

### Unregister a key:
```erlang
device_registry:unregister(<<"ping">>).
```

## 📣 Notes

- Messages must contain: `#{from, key, data}`
- All routing is done based on `key` → `device_registry`
- ETS is local per node (register on the node doing the work)