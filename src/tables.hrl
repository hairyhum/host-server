-record(todo_device, {login, token}). % Bag
-record(todo_user, {login, pin, email, role}). % Set
-record(todo_entity, {id, login}). % Set
-record(todo_field, {entity_id, name, value, clocks}). % Bag
-record(todo_clock, {field_spec, token, clock}). % Bag

