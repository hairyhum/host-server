-record(todo_device, {token, login}). % Set
-record(todo_user, {login, pin, email, role}). % Set
-record(todo_entity, {login, id}). % Set
-record(todo_field, {entity, name, value, clocks}). % Bag
-record(todo_clock, {field_spec, token, clock}). % Bag

