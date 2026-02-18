# Ada Package and Library List for GNAT Studio

**Last Updated:** February 2026  
**Target Environment:** GNAT Studio / GNAT Community / GNAT Pro

This document provides a comprehensive list of Ada packages and libraries available in the GNAT ecosystem. Packages are organized by functional category with descriptions of their primary usage.

---

## Table of Contents
1. [Ada Standard Libraries](#ada-standard-libraries)
2. [Container Libraries](#container-libraries)
3. [File I/O and Directory Operations](#file-io-and-directory-operations)
4. [String and Text Processing](#string-and-text-processing)
5. [Mathematical and Numerical Libraries](#mathematical-and-numerical-libraries)
6. [Tasking and Real-Time Systems](#tasking-and-real-time-systems)
7. [GNAT-Specific Libraries](#gnat-specific-libraries)
8. [GUI and Graphics Libraries](#gui-and-graphics-libraries)
9. [Web and Networking Libraries](#web-and-networking-libraries)
10. [Database and Persistence](#database-and-persistence)
11. [XML and Unicode Processing](#xml-and-unicode-processing)
12. [Testing and Verification](#testing-and-verification)
13. [Bindings to External Libraries](#bindings-to-external-libraries)

---

## Ada Standard Libraries

The Ada Reference Manual (Annex A) defines the standard libraries that all Ada compilers must provide. GNAT fully implements these facilities .

| Package | Usage Description |
|---------|-------------------|
| `Ada` | Parent package for all standard library packages; usually implicitly included  |
| `Ada.Assertions` | Provides `Assert` subprograms and `Assertion_Error` exception  |
| `Ada.Calendar` | Time of day access, date/time manipulation  |
| `Ada.Calendar.Arithmetic` | Additional arithmetic operations for Calendar  |
| `Ada.Calendar.Formatting` | Formatting operations for Calendar values  |
| `Ada.Calendar.Time_Zones` | Time zone handling for Calendar operations  |
| `Ada.Characters.Handling` | Character classification (letters, digits, etc.)  |
| `Ada.Characters.Latin_1` | Definitions for Latin-1 character set  |
| `Ada.Command_Line` | Access to command-line arguments and program name  |
| `Ada.Environment_Variables` | Access to environment variables  |
| `Ada.Exceptions` | Exception information and handling as data objects  |
| `Ada.Finalization` | Controlled types support (initialization/finalization)  |
| `Ada.Interrupts` | Interrupt handling and signal management  |
| `Ada.IO_Exceptions` | Exception definitions for I/O operations  |
| `Ada.Locales` | Locale information and internationalization support  |
| `Ada.Numerics` | Mathematical constants and numerical functions  |
| `Ada.Real_Time` | Real-time clocks, delays, and scheduling  |
| `Ada.Storage_IO` | Support for storage-based I/O operations |
| `Ada.Strings` | String handling foundations and generic operations |
| `Ada.Strings.Bounded` | Bounded-length string handling |
| `Ada.Strings.Fixed` | Fixed-length string operations |
| `Ada.Strings.Unbounded` | Unbounded string handling (dynamic length) |
| `Ada.Strings.Wide_Unbounded` | Wide character unbounded strings |
| `Ada.Strings.Wide_Wide_Unbounded` | Wide-wide character unbounded strings |
| `Ada.Synchronous_Task_Control` | Low-level task synchronization primitives |
| `Ada.Tags` | Runtime type identification for tagged types |
| `Ada.Task_Identification` | Task identification and inspection |
| `Ada.Task_Termination` | Task termination handling |

---

## Container Libraries

The Ada standard containers provide generic data structures for efficient data management .

| Package | Usage Description |
|---------|-------------------|
| `Ada.Containers` | Top-level package with basic container definitions  |
| `Ada.Containers.Vectors` | Resizable one-dimensional arrays  |
| `Ada.Containers.Doubly_Linked_Lists` | Bidirectional linked lists  |
| `Ada.Containers.Hashed_Maps` | Hash table-based mapping  |
| `Ada.Containers.Ordered_Maps` | Tree-based ordered mapping  |
| `Ada.Containers.Hashed_Sets` | Hash table-based sets  |
| `Ada.Containers.Ordered_Sets` | Tree-based ordered sets  |
| `Ada.Containers.Multiway_Trees` | General tree structures  |
| `Ada.Containers.Indefinite_Vectors` | Vectors for indefinite element types  |
| `Ada.Containers.Indefinite_Doubly_Linked_Lists` | Lists for indefinite element types  |
| `Ada.Containers.Indefinite_Hashed_Maps` | Hash maps for indefinite element types  |
| `Ada.Containers.Indefinite_Ordered_Maps` | Ordered maps for indefinite element types  |
| `Ada.Containers.Indefinite_Hashed_Sets` | Hash sets for indefinite element types  |
| `Ada.Containers.Indefinite_Ordered_Sets` | Ordered sets for indefinite element types  |
| `Ada.Containers.Indefinite_Holders` | Holder containers for indefinite types  |
| `Ada.Containers.Bounded_Synchronized_Queues` | Bounded thread-safe queues  |
| `Ada.Containers.Unbounded_Synchronized_Queues` | Unbounded thread-safe queues  |
| `Ada.Containers.Bounded_Priority_Queues` | Bounded priority queues  |
| `Ada.Containers.Unbounded_Priority_Queues` | Unbounded priority queues  |

---

## File I/O and Directory Operations

The Ada standard provides comprehensive file I/O capabilities .

| Package | Usage Description |
|---------|-------------------|
| `Ada.Text_IO` | Text file I/O and console I/O  |
| `Ada.Text_IO.Text_Streams` | Stream interface for Text_IO files |
| `Ada.Wide_Text_IO` | Wide character text I/O |
| `Ada.Wide_Wide_Text_IO` | Wide-wide character text I/O |
| `Ada.Sequential_IO` | Binary file I/O for single data types  |
| `Ada.Direct_IO` | Direct access binary I/O for definite types  |
| `Ada.Streams` | Root package for stream I/O operations  |
| `Ada.Streams.Stream_IO` | Stream-based binary I/O supporting multiple data types  |
| `Ada.Directories` | Directory operations (create, delete, search)  |
| `Ada.Directories.Hierarchical_File_Names` | Hierarchical pathname operations  |
| `Ada.Text_IO.Bounded_IO` | Bounded string I/O operations |
| `Ada.Text_IO.Unbounded_IO` | Unbounded string I/O operations |
| `Ada.Text_IO.Enumeration_IO` | Enumeration type I/O |
| `Ada.Text_IO.Integer_IO` | Integer type I/O |
| `Ada.Text_IO.Float_IO` | Floating-point type I/O  |
| `Ada.Text_IO.Fixed_IO` | Fixed-point type I/O |
| `Ada.Text_IO.Decimal_IO` | Decimal type I/O |
| `Ada.Text_IO.Modular_IO` | Modular integer type I/O |
| `Ada.Wide_Text_IO.Wide_Bounded_IO` | Wide bounded string I/O |
| `Ada.Wide_Text_IO.Wide_Unbounded_IO` | Wide unbounded string I/O |
| `Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO` | Wide-wide bounded string I/O |
| `Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO` | Wide-wide unbounded string I/O |

---

## String and Text Processing

| Package | Usage Description |
|---------|-------------------|
| `Ada.Strings.Maps` | Character mapping and sets |
| `Ada.Strings.Maps.Constants` | Predefined character mappings |
| `Ada.Strings.Equal_Case_Insensitive` | Case-insensitive string comparison |
| `Ada.Strings.Hash` | Hash function for strings |
| `Ada.Strings.Less_Case_Insensitive` | Case-insensitive ordering |
| `Ada.Strings.Unbounded.Text_IO` | Text I/O for unbounded strings |
| `Ada.Strings.Wide_Unbounded.Wide_Text_IO` | Wide text I/O for unbounded strings |
| `Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO` | Wide-wide text I/O for unbounded strings |
| `Ada.Strings.Wide_Maps` | Character maps for wide characters |
| `Ada.Strings.Wide_Wide_Maps` | Character maps for wide-wide characters |
| `Ada.Strings.UTF_Encoding` | UTF-8, UTF-16, UTF-32 encoding/decoding |
| `Ada.Strings.UTF_Encoding.Conversions` | Conversion between UTF encodings |
| `Ada.Strings.UTF_Encoding.Strings` | UTF string handling |
| `Ada.Strings.UTF_Encoding.Wide_Strings` | UTF handling for wide strings |
| `Ada.Strings.UTF_Encoding.Wide_Wide_Strings` | UTF handling for wide-wide strings |

---

## Mathematical and Numerical Libraries

The `Ada.Numerics` hierarchy provides mathematical functions and numerical computation support .

| Package | Usage Description |
|---------|-------------------|
| `Ada.Numerics` | Mathematical constants (Pi, e, etc.) |
| `Ada.Numerics.Generic_Elementary_Functions` | Generic elementary functions (sin, cos, log, exp, sqrt) |
| `Ada.Numerics.Elementary_Functions` | Instantiation for Float |
| `Ada.Numerics.Long_Elementary_Functions` | Instantiation for Long_Float |
| `Ada.Numerics.Short_Elementary_Functions` | Instantiation for Short_Float |
| `Ada.Numerics.Generic_Complex_Types` | Generic complex number arithmetic |
| `Ada.Numerics.Complex_Types` | Complex types instantiated for Float |
| `Ada.Numerics.Complex_Elementary_Functions` | Complex elementary functions |
| `Ada.Numerics.Generic_Real_Arrays` | Generic real vector and matrix operations |
| `Ada.Numerics.Real_Arrays` | Real arrays instantiated for Float |
| `Ada.Numerics.Generic_Complex_Arrays` | Generic complex vector and matrix operations |
| `Ada.Numerics.Complex_Arrays` | Complex arrays instantiated for Float |
| `Ada.Numerics.Float_Random` | Random number generation (Float) |
| `Ada.Numerics.Discrete_Random` | Generic discrete random number generation |
| `Ada.Numerics.Long_Long_Elementary_Functions` | Elementary functions for Long_Long_Float |
| `Ada.Complex_Text_IO` | Text I/O for complex numbers  |

---

## Tasking and Real-Time Systems

Ada's built-in tasking model is extended by these real-time and synchronization packages .

| Package | Usage Description |
|---------|-------------------|
| `Ada.Dynamic_Priorities` | Dynamic task priority adjustment  |
| `Ada.Execution_Time` | CPU time measurement for tasks  |
| `Ada.Real_Time.Timing_Events` | Real-time event scheduling |
| `Ada.Synchronous_ Task_Control.EDF` | Earliest Deadline First synchronization |
| `Ada.Dispatching` | Task dispatching policy definitions  |
| `Ada.Dispatching.Round_Robin` | Round-robin dispatching (not always implemented)  |
| `Ada.Dispatching.EDF` | EDF dispatching (not always implemented)  |
| `Ada.Dispatching.Non_Preemptive` | Non-preemptive dispatching (not always implemented)  |
| `Ada.Asynchronous_Task_Control` | Low-level task control (implementation-dependent)  |
| `System.Tasking` | GNAT low-level tasking support (implementation-defined) |
| `System.Tasking.Restrictions` | Tasking restrictions for Ravenscar profile |

---

## GNAT-Specific Libraries

These libraries are specific to the GNAT compiler suite and provide additional functionality beyond the Ada standard .

| Library | Usage Description |
|---------|-------------------|
| `GNAT` | Root package for GNAT-specific extensions |
| `GNAT.OS_Lib` | Operating system interface (file operations, processes, environment) |
| `GNAT.Command_Line` | Advanced command-line parsing |
| `GNAT.Compiler_Version` | Access to compiler version information |
| `GNAT.Current_Exception` | Access to current exception information |
| `GNAT.Debug_Utilities` | Debugging support utilities |
| `GNAT.Directory_Operations` | Advanced directory operations |
| `GNAT.Dynamic_HTables` | Dynamic hash tables |
| `GNAT.Exception_Traces` | Exception traceback control |
| `GNAT.Heap_Sort` | Heap sort implementation |
| `GNAT.Heap_Sort_G` | Generic heap sort |
| `GNAT.HTable` | Static hash table implementation |
| `GNAT.IO` | Simple I/O operations |
| `GNAT.IO_Aux` | Auxiliary I/O operations |
| `GNAT.Lock_Files` | File-based locking mechanism |
| `GNAT.MD5` | MD5 message digest computation |
| `GNAT.Memory_Dump` | Memory content dumping |
| `GNAT.Most_Recent_Exception` | Access to most recently raised exception |
| `GNAT.Regexp` | Regular expression matching |
| `GNAT.Registry` | Windows registry access (Windows only) |
| `GNAT.SHA1` | SHA-1 hash computation |
| `GNAT.SHA224` | SHA-224 hash computation |
| `GNAT.SHA256` | SHA-256 hash computation |
| `GNAT.SHA384` | SHA-384 hash computation |
| `GNAT.SHA512` | SHA-512 hash computation |
| `GNAT.Signals` | Signal handling |
| `GNAT.Sockets` | Network socket interface |
| `GNAT.Source_Info` | Access to source code information (file, line, function) |
| `GNAT.Strings` | String handling utilities |
| `GNAT.Table` | Generic table implementation |
| `GNAT.Task_Lock` | Task-safe locking |
| `GNAT.Time_Stamp` | Time stamp handling |
| `GNAT.Traceback` | Stack traceback generation |
| `GNAT.Traceback.Symbolic` | Symbolic stack traceback |
| `GNAT.UTF_32` | UTF-32 handling utilities |

---

## GNAT Component Collection (GNATcoll)

GNATcoll is a comprehensive set of reusable components and bindings .

| Library | Usage Description |
|---------|-------------------|
| `GNATCOLL` | Root package for GNAT Component Collection |
| `GNATCOLL.Arg_Lists` | Command-line argument list handling |
| `GNATCOLL.Boyer_Moore` | Boyer-Moore string search algorithm |
| `GNATCOLL.Code_Peel` | Code peeling utilities |
| `GNATCOLL.Config` | Configuration file parsing |
| `GNATCOLL.Conditional_Variables` | Condition variable synchronization |
| `GNATCOLL.Console` | Console/terminal handling |
| `GNATCOLL.Cpp` | C++ integration support |
| `GNATCOLL.Emacs` | Emacs integration utilities |
| `GNATCOLL.Encodings` | Character encoding conversion |
| `GNATCOLL.File_Indexer` | File indexing system |
| `GNATCOLL.File_Server` | Simple file server implementation |
| `GNATCOLL.Filesystem` | Advanced filesystem operations |
| `GNATCOLL.Filesystem.Temp` | Temporary file/directory management |
| `GNATCOLL.Iconv` | Character set conversion (binding to libiconv)  |
| `GNATCOLL.Iconv.Encodings` | Higher-level encoding conversions |
| `GNATCOLL.JSON` | JSON parsing and generation |
| `GNATCOLL.JSON.Utility` | JSON utility functions |
| `GNATCOLL.Lexer` | Lexical analysis |
| `GNATCOLL.Lockfile` | Lock file management |
| `GNATCOLL.Logger` | Logging framework |
| `GNATCOLL.Memory` | Memory management utilities |
| `GNATCOLL.Mmap` | Memory-mapped file I/O |
| `GNATCOLL.OS` | Operating system abstraction |
| `GNATCOLL.OS.Constants` | OS-specific constants |
| `GNATCOLL.Projects` | Project file handling (GPRbuild project files) |
| `GNATCOLL.Projects.Ada_Main` | Ada main program detection |
| `GNATCOLL.Python` | Python integration |
| `GNATCOLL.Readline` | Command-line editing (binding to GNU Readline)  |
| `GNATCOLL.Refcount` | Reference counting utilities |
| `GNATCOLL.Scripts` | Scripting language integration |
| `GNATCOLL.Scripts.Shell` | Shell script interpreter |
| `GNATCOLL.Semaphores` | Semaphore synchronization |
| `GNATCOLL.String_Builders` | Efficient string building |
| `GNATCOLL.Strings` | Advanced string handling |
| `GNATCOLL.Symbols` | Symbol table management |
| `GNATCOLL.Syslog` | System logging (binding to syslog)  |
| `GNATCOLL.Terminal` | Terminal control |
| `GNATCOLL.Traces` | Tracing and debugging framework |
| `GNATCOLL.Traces.Syslog` | Syslog integration for traces |
| `GNATCOLL.Tribooleans` | Three-state boolean logic |
| `GNATCOLL.Utils` | General utility functions |
| `GNATCOLL.VADER` | VADER tool integration |
| `GNATCOLL.VFS` | Virtual File System abstraction |
| `GNATCOLL.VFS_Utils` | VFS utility functions |

---

## GUI and Graphics Libraries

| Library | Usage Description |
|---------|-------------------|
| `GtkAda` | Complete Ada binding for GTK+ toolkit  |
| `GtkAda.Glade` | Glade UI builder integration |
| `GtkAda.Source_View` | Source code editing widget |
| `GtkAda.Webkit` | WebKit embedding |
| `Gnoga` | GUI for web applications (HTML5/CGI/WebSocket) |
| `QtAda` | Qt binding (varying availability) |
| `Claw` | Windows native GUI binding |
| `TASH` | Tcl/Tk binding |
| `AdaCGI` | CGI web interface library |
| `AWS.Templates` | HTML template system  |

---

## Web and Networking Libraries

| Library | Usage Description |
|---------|-------------------|
| `AWS` (Ada Web Server) | Complete web server and client framework  |
| `AWS.Client` | HTTP client implementation |
| `AWS.Server` | HTTP server implementation |
| `AWS.Services` | Web services support |
| `AWS.Services.Web_Block` | Web block services |
| `AWS.Services. SOAP` | SOAP web services  |
| `AWS.Services.REST` | RESTful services  |
| `AWS.Dispatchers` | Request dispatchers |
| `AWS.Parameters` | HTTP parameter handling |
| `AWS.Messages` | HTTP message handling |
| `AWS.Resources` | Resource management |
| `AWS.Response` | HTTP response handling |
| `AWS.Status` | Server status monitoring |
| `AWS.Session` | Session management |
| `AWS.Utils` | Web utilities |
| `GNAT.Sockets` | Low-level socket interface |
| `GNAT.Sockets.Constants` | Socket constants |
| `GNAT.Sockets.Thin` | Thin socket binding |
| `GNAT.Sockets.Thin_Common` | Common socket definitions |
| `PolyORB` | CORBA middleware implementation  |
| `PolyORB.Utils` | CORBA utilities |
| `Anet` | Networking library |
| `Adacloud` | Cloud service integration |

---

## Database and Persistence

| Library | Usage Description |
|---------|-------------------|
| `GNATCOLL.SQL` | SQL database access framework  |
| `GNATCOLL.SQL.Exec` | SQL execution engine |
| `GNATCOLL.SQL.Internal` | SQL internal implementation |
| `GNATCOLL.SQL.Oracle` | Oracle database support |
| `GNATCOLL.SQL.Postgres` | PostgreSQL support |
| `GNATCOLL.SQL.SQLite` | SQLite database support |
| `GNATCOLL.SQL.Mysql` | MySQL support |
| `GNATCOLL.SQL.Sessions` | Database session management |
| `GNATCOLL.Storage_Pools` | Custom storage pool implementations |
| `ADO` (Ada Database Objects) | Object-relational mapping |
| `ADO.Queries` | Database queries |
| `ADO.Schemas` | Database schema handling |
| `Ahven` | Lightweight unit testing (also for testing) |
| `GNADE` | GNAT Ada Database Environment |
| `Berkeley_DB` | Berkeley DB binding |

---

## XML and Unicode Processing

| Library | Usage Description |
|---------|-------------------|
| `XML/Ada` | Full XML processing library  |
| `XML/Ada.DOM` | DOM (Document Object Model) implementation |
| `XML/Ada.SAX` | SAX (Simple API for XML) parsing |
| `XML/Ada.Input_Sources` | XML input source handling |
| `XML/Ada.Schema` | XML schema validation |
| `XML/Ada.Unicode` | Unicode support for XML |
| `Unicode` | Unicode character handling |
| `Unicode.CCS` | Coded Character Sets |
| `Unicode.CCS.ASCII` | ASCII handling |
| `Unicode.CCS.ISO_8859_1` | ISO 8859-1 handling |
| `Unicode.CCS.UTF_8` | UTF-8 encoding |
| `Unicode.CCS.UTF_16` | UTF-16 encoding |
| `Unicode.CCS.UTF_32` | UTF-32 encoding |
| `Unicode.Names` | Unicode character names |
| `Matreshka` | Multipurpose framework with XML/Unicode support  |
| `Matreshka.LEXML` | Lightweight XML processor |
| `Matreshka.XML` | XML processing |
| `Matreshka.XML.Schema` | XML schema |
| `Matreshka.XSLT` | XSLT transformations |
| `Matreshka.Regex` | Regular expressions |
| `Matreshka.URI` | URI handling |

---

## Testing and Verification

| Library/Tool | Usage Description |
|--------------|-------------------|
| `AUnit` | Unit testing framework for Ada  |
| `AUnit.Repetitive_Test` | Repetitive test execution |
| `AUnit.Simple_Test_Cases` | Simple test case implementation |
| `AUnit.Test_Cases` | Test case framework |
| `AUnit.Test_Filters` | Test filtering |
| `AUnit.Test_Suites` | Test suite composition |
| `GNATtest` | Automated testing framework  |
| `GNATtest.Assert` | Test assertions |
| `GNATCOLL.Test` | Testing utilities |
| `AdaTest` | Unit testing with aspect-based specifications |
| `GNATprove` | Formal verification for Ada/SPARK  |
| `GNATstack` | Stack usage analysis  |
| `GNATcoverage` | Code coverage measurement  |
| `GNATcheck` | Style checking and coding standards  |
| `AdaControl` | Source code checking and metrics  |
| `SPARK` | Formal verification toolset  |
| `SPARK.Lemmas` | Mathematical lemmas for verification |
| `SPARK.Big_Integers` | Arbitrary-precision integers for SPARK |

---

## Bindings to External Libraries

| Library | Usage Description |
|---------|-------------------|
| `FLORIST` | POSIX Ada binding  |
| `FLORIST.Realtime` | POSIX real-time extensions |
| `FLORIST.Threads` | POSIX threads binding |
| `GtkAda` | GTK+ binding (see GUI section) |
| `GNUTLS` | TLS/SSL binding |
| `Zlib` | Compression library binding |
| `Zlib.Thin` | Thin binding to zlib |
| `PCRE` | Perl-compatible regular expressions |
| `Curses` | Curses terminal control binding |
| `Curses.Forms` | Forms library for curses |
| `Curses.Menus` | Menus library for curses |
| `Curses.Panels` | Panels library for curses |
| `OpenGL` | OpenGL graphics binding |
| `OpenGL.Ada` | Alternative OpenGL binding |
| `SDL` | Simple DirectMedia Layer binding |
| `SDL.Ada` | SDL multimedia binding |
| `Allegro` | Allegro game library binding |
| `ODBC` | ODBC database binding |
| `Oracle` | Oracle Call Interface binding |
| `MySQL` | MySQL client library binding |
| `PostgreSQL` | PostgreSQL client binding |
| `SQLite` | SQLite embedded database binding |
| `Readline` | GNU Readline binding  |
| `Iconv` | Character set conversion binding  |
| `Syslog` | System logging binding  |

---

## Notes on Using These Libraries in GNAT Studio

### Adding Libraries to Your Project

In GNAT Studio, libraries can be added to your project through:

1. **Project Properties**: Right-click project → Properties → Libraries
2. **Project File (.gpr)**: Directly edit your project file to add with clauses
3. **External Dependencies**: Use `with "library_name";` in your project file

### Library Versions

Library availability may vary depending on:
- GNAT Studio version (Community vs. Pro)
- Target platform (Windows, Linux, macOS, embedded)
- Installed additional components

### Documentation Access

Most libraries include documentation accessible through:
- GNAT Studio Help menu
- AdaCore documentation portal 
- Library-specific user guides

---