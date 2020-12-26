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
  a database table. There need not be a one-to-one relationship between a domain
  entity, a database entity, and the presentation entity.

- domain algorithms. There are quite a few of them.

The dependency configurator (`app` directory) is responsible for configuring and
connecting other layers, and running the application. It depends on the all
other layers and hence is quite dirty, which is ok, since it must not contain
too much logic. A typical module of the layer is `app/Main.hs`. Additionally, it
reads configuration files and command line parameters.

There is the dependency rule: higher-level layers shall not depent on
lower-level ones. Therefore, the business logic layer shall not depend on the
presentation and the data source layers, so that it may be placed in a different
library or an executable. The presentation and the data source are on the same
level, but they do not depent on each other too. They may depend on the business
logic layer, and they often need to. The dependency rule involves very important
consequences:

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

- `Web.Application` describes a web application interface that all handlers can
  use. It was not so important to create, but it turned out to be a simple and
  useful experiment. It mostly duplicates `WAI` library (and actually uses it
  under the hood), but has the following advantages:

  - we can control `Web.Application`, as opposed to `WAI`, as it's an external
    library. Using our own interface, we can easily migrate to some other web
    server or web server interface, having created an appropriate compatibility
    layer.

  - as compared with `WAI`, `Web.Application` includes things which are
    important and convenient for this application and does not include ones that
    aren't. E.g. it does not support the streaming interface or chunked load of
    request bodies. Instead, it supports loading a request body no longer than a
    specific limit.

- `AppURI` - ADT-represented URI. It allows passing information of a specific
  endpoint, abstracted from the textual representation of URI - e.g. `ImageURI`
  constructor corresponds to `/images/{id}` and contains the identifier of an
  image. So, an URI path may be changed, while keeping links in the code valid.
  Think of `AppURI` as a module with two functions: one parses an URI into
  `AppURI` and another performs the reverse mapping.

- `Web.Router` module providers `Router` type which determines a handler to
  process a request, basing on an AppURI, and detects errors like an unknown
  resource or an unsupported HTTP method. Since `AppURI` performs URI
  recognition, there isn't too much work.

- handlers (`Web.Handler.*`) are responsible for decoding a request,
  constructing a higher-level request to the core, passing it, and handling the
  result. Note that handlers must not depend on the _response_ format,
  _response_ headers or status codes - they delegate data output to an injected
  presenter function. It is important to reduce handlers complexity and make
  them more concentrated on the input rather than the output.

  Note that each handler is run in a monad parameter like `m`, not `IO`. It is
  important to run handlers in a, say, `Transaction` monad, so that all SQL code
  is run in a single transaction.

- `Web.EntryPoint` assembles the whole web part of the application. It uses the
  router to process requests. Additionally, it handles application exceptions
  and outputs corresponding HTTP statuses and responses.

- `Web.QueryParameter` provides a parser applicative functor to providing a
  convenient and efficient service for parsing HTTP query parameters. Efficiency
  is not a real goal and is concerned mostly for fun. And the parser is not
  really efficient in simple cases - `lookup` over a list would be better.

- the representations (`src/Web/Representation`) and the representation builder.
  They are responsible for converting domain data to the form, suitable for
  JSON-encoding and output to the user. Representations reduce duplication of
  code performing the conversion. The application has a substantial REST-like
  API with a lot of entities, which are often nested and repeated in different
  parts of API. Representations are often named like `UserRep` for the
  corresponding `User` entity. The representation builder is mostly a reader
  monad which can be provided with functions like `renderAppURI`.

- `Web.Presenter` is responsible for presenting a request handling result,
  usually a REST API entity response. Most presenters are simple, they just use
  representations and add HTTP status code and headers. `Web.Presenter.Error`
  presents errors - mostly thrown exceptions.

## Warp and WAI

`FrontEnd.WAI` implements `Web.Application` interface via `WAI`. So, only
`FrontEnd.WAI` and `Main` know something about `WAI` or `Warp`.

## Business logic

Core entities (`Core.User`, `Core.Author`, etc) describe domain data. They does
not necessarily map to the corresponding entities in the database or
presentation or duplicate them. These modules could also contain _domain_
algorithms - ones that are specific to the problem space, not
application-specific.

Most modules in `Core` consist of application-specific script functions. They
are: authentication, authorization, use case interactors, parsing paginated
output specification.

Note that most core functions performing actions should be parameterized with a
monad, like `Monad m => A -> m B`. `IO` monad should be avoided, and all
external effects should be provided via a corresponding `Handle` field, executed
in the parameter monad. It restricts undesirable side-effects and allows running
business logic code in any monad, e.g. `Transaction`.

Business logic code is unaware of database transactions and does not depend on
the transaction machinery. It should be run in a single transaction.

`Core.Authentication` parses credentials, checks them, and possibly result in an
`AuthenticatedUser`, which is later used for checking the user identity and
permissions.

`Core.Authorization` is a helper module for checking user permissions and
throwing an exception on failure.

`Core.Pagination` describe pagination specification types and a parser logic.
The logic is tiny, but non-trivial.

`Core.Interactor.*` are use case interactor modules. Each performs one abstract
use case script. Each interactor module must consist of an only function `run`,
`Handle` type and optionally other types.

## Database

`Database` module reexports all data API, suitable for injecting into core
modules. Additionally `Database.Service.Primitives` is needed to run a database
transaction, but this is only needed somewhere in `Main`, where dependencies are
injected.

`Database.Service.*` offer basic API for implementing `Database.Logic.*` which
contains database logic.

`Database.Service.ConnectionManager` allows running a function with a given
database connection. Its goal to isolate other code from the knowledge of how
connections are created. The module can be improved by adding a connection pool
logic.

`Database.Service.Primitive` is the most basic database module. It supports
`Session` and `Transaction` monads where transactions can be run. Each SQL
operator may be output into a log.

`Database.Columns` contains `Columns` type - an applicative decoder of database
table column names. It allows to reduce duplication of column names in SQL
`SELECT` queries and to decode database data into higher-level types. It is not
enough, though, to significantly reduce duplication of SQL code in the
application :(.

`Database.SQLBuilder` contains `Builder` monoid which may be rendered into a
simple `SQL` string with auto-numbered inline parameter placeholders and encoded
parameters. It simplifies building long, complex, and dynamic SQL querise.
`Database.SQLBuilders` contains a lot of such builders.

## Main

`app` directory contains modules to configure, connect and run other layers.

`Config.IO` performs reading and format-specific parsing a config file and
command line parameters. It should not contain default values or decision
making. If we migrate to another config format, we will modify this module, not
others.

`Config` reads raw configuration data from `Config.IO` and transforms it into a
more useful, higher-level form. It provides default values if some parameters
are missing, etc.

`Main` creates all dependencies, runs the Warp and the logger. Additionally, it
contains tiny pieces of logic, such as adding session id into logs, that should
be more appropriate to move into other modules. If they get more complex, we
probably will do it.

`Handlers` is full of template code to create handlers. There is a function for
each handler to create all necessary Handles. The module should not contain
complex logic because of its size - you will see that handlers type may change
during configuration, but this should be performed in another place, say,
`Main`.
