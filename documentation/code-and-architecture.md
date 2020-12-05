# Code and architecture

The document outlines the program architecture and code guidelines.

# The Clean Architecture

The project tries to follow the Clean Architecture, although does not conform to
it perfectly because of the small size of the application and the lack of
knowledge. The following rules should be considered.

Layering. There are at least four independent layers: the presentation (aka
web), the business logic (aka core), the data source (aka data layer or
database), and the dependency configurator (aka main). The business logic layer
is higher than others. The data source and the presentation are lower: they
provide services for the business logic layer. The dependency configurator is
the lowest.

The presentation layer (`src/Web` directory) is responsible for presenting
information from the core to the user and, in the opposite, interpreting user
commands and passing them to the core. Different presentation layers (web,
mobile GUI, CLI, desktop GUI) may be used to port the application to different
user interfaces and platforms, keeping the business logic and the data source
code unmodified.

The data source layer (`src/Database`, `src/Gateway`) is responsible for
providing services to external systems and hardware that the application needs
to use: storing information persistently, providing services to payment systems
and hardware (clock, bluetooth, networking, etc). Different data source layers
may be used to port the application to different storage types (RDBMS, NoSQL,
plain files, backends), different hardware, different network protocols etc,
keeping the business logic and the presentation code unmodified.

The business logic layer (`src/Core`) consists of things that are inherent to
the nature of the application and independent of the input/output. Note that the
presentation and the data source are merely input/output plug-ins of the
business logic: they input information from a user/external system and output it
back. The business logic should probably consist of everything that should not
be modified when UI, storage type, hardware or networking changes. They are the
following:

- use cases (mostly `src/Core/Interactor`). They are application scripts
  describing actions that the user of the system can perform, _abstracted from
  the exact kind of UI and data sources_. Each use case is implemented by a
  separate interactor module and usually consists of an only function `run`,
  although there are exceptions. Authentication is a use-case too.

- domain entities. They are data structures (ADT), suitable for writing
  algorithms and implementing business rules. They do not depend on the storage
  or display format. It is ok when a domain entity has a name, field names,
  field amount, field types different from the corresponding REST API entity or
  a database table.

- domain algorithms. There are quite a few of them.

The dependency configurator (`app` directory) is responsible for configuring and
connecting other layers, and running the application. It depends on the all
other layers and hence is quite dirty, which is ok, since it must not contain
any logic. A typical module of the layer is `app/Main.hs`.

The dependency rule: higher-level layers shall not depent on lower-level ones.
Therefore, the business logic layer does not depend on the presentation and the
data source, so that it may be placed in a different library or a process. The
presentation and the data source are on the same level, but they do not depent
on each other too. They can depend on the business logic layer. The dependency
rule involves some important consequences:

- the business logic shall not obey the database or the presentation. Domain
  entity structure shall not depend on the database schema or REST API entity
  fields, but the opposite: database schema must be designed after domain
  entities are.

- Business logic dependencies should be convenient to be used by the business
  logic, not by the data source layer. Forget the database when writing
  algorithms in the business logic layer.

- There must not be SQL code or JSON in the business logic layer. All services
  that the business logic needs from the data layer should be just functions,
  e.g. `findAllUsersOlderThan` or `getNewsOfAuthors`.

# Program structure

## Presentation

The presentation layer implements  REST-like API. It consists of the following
parts:

- `AppURI` - ADT-represented URI. It allows passing information of a specific
  endpoint, abstracted from the textual representation of URI - e.g. `ImageURI`
  constructor corresponds to `/images/{id}` and contains the identifier of an
  image. So, URI path may change without affecting links in the code.
  
- the router determines a handler to process a request, basing on an AppURI, and
  detects errors like an unknown resource or an unsupported HTTP method.
  
- handlers (`src/Web/Handler`) are responsible for decoding a request,
  constructing a higher-level request to the core, passing it, and handling the
  result. Note that handlers must not depent on the response format, response
  headers or status codes - they delegate it to an injected presenter function.
  It is important to reduce handlers complexity and make them more concentrated
  on the input rather than the output. Handlers know the request format and can
  see request headers.

- the entry point is a module to assemble the whole web application. It uses the
  router to process requests. Additionally, it handles application exceptions
  and outputs corresponding HTTP statuses and responses.

- the query parser is responsible for providing a convenient and efficient
  service for parsing query parameters. Efficiency is not a real goal and is
  concerned mostly for fun. And the parser is not really efficient in simple
  cases - `lookup` over a list would be better.

- the representations (`src/Web/Representation`) and the representation builder.
  They are responsible for converting data to the form suitable for
  JSON-encoding and for reducing duplication of code performing the conversion.
  The application has a substantial REST-like API with a lot of entities, which
  are often nested and repeated in different parts of API. Representations are
  often named like `UserRep` for the corresponding `User` entity.

- presenters are responsible for presenting a successful result of handler's
  work, usually a REST API entity response. Presenters are simple, they just use
  representations and add HTTP status code and headers.
