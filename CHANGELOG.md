# Revision history for <strong>iTracker</strong>

## 0.1.0.0 -- 2022-02-22

- First version. Released on an unsuspecting world.

| No  | Story                                      | Request                                                                                                                                       | Response                                                                                         | Tracker          |
| --- | ------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ | ---------------- |
| 1   | as a user I can create an account          | `username`, `email`, `password`                                                                                                               | message that success/failed                                                                      | <!-- tracker --> |
| 2   | as a user I can login                      | `username`, `password`                                                                                                                        | message that success/failed                                                                      | ❌               |
| 3   | as a user I can register my company        | `company name`, `company code`, `author`                                                                                                      | create the directory and write (also return as message) that the company is successfully created | <!-- tracker --> |
| 4   | as a user I can make a history             | `message`, `PIC of department name`, `status (I - in, P - progress, O - out, E - error, D - done)`, `author`, `date`, `password (to confirm)` | add new line of messages, and return success message                                             | <!-- tracker --> |
| 5   | as a user I can track my company histories | `history code`                                                                                                                                | return message if exist                                                                          | <!-- tracker --> |
| 6   | there will be a file to track changes      | -                                                                                                                                             | -                                                                                                | <!-- tracker --> |

```hs
-- message

|   date   |  code   | supplier name |         address        |        email       |      phone     |   product   | quantity | @ price | status |
-------------------------------------------------------------------------------------------------------------------------------------------------
| 20/12/12 | XEje4d3 |   Unilever    | Jakarta Pusat, Unit 32 | hello@unilever.com | (+21) 534 2434 |  Ultra Milk |  4 Boxes |   6500  |    I   |
```
