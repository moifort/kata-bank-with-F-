# Kata bank F#

Given a client makes a deposit of 500 on 10-01-2012

And a deposit of 500 on 13-01-2012

And a withdrawal of 100 on 14-01-2012

And a deposit of 1000 on 13-01-2012

When she prints her bank statement

Then she would see

```log
DATE       | AMOUNT  | BALANCE
01/04/2014 | 500.00 | 1000.00
01/04/2014 | -100.00 | 900.00
01/04/2014 | 1000.00 | 1900.00 
```


# TODO
- [ ] Add date
- [ ] Add Result
- [ ] Add Dependency Injection
- [ ] Double rail way