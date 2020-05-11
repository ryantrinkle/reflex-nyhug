# Configuration Values

## `config/backend/clientSessionKey`

This is the MAC key that will be passed to [Web.ClientSession.initKey](http://hackage.haskell.org/package/clientsession-0.9.1.2/docs/Web-ClientSession.html#v:initKey).
This file must be exactly 96 bytes long, and should be chosen uniformly from all 96-byte long binary bytestrings.
One way of generating a key is the executing the following command in `obelisk/skeleton` or another appropriate directory:

```
dd bs=96 count=1 if=/dev/urandom of=config/backend/clientSessionKey
```

## `config/backend/email`

Like many apps, we allow users to sign up via email. TODO MORE

To do this, the backend sends mail over SMPT to a mail server which then sends it to the user.
`config/backend/email` stores the information the backend needs to connect to that mail server.
It should look something like this:

```json
{
  "_emailConfig_emailAuth": {
    "_emailAuth_authType": "LOGIN",
    "_emailAuth_username": "<a-username>",
    "_emailAuth_password": "<a-password>"
  },
  "_emailConfig_hostname": "smtp.example.com",
  "_emailConfig_port": 2525,
  "_emailConfig_protocol": "SMTPProtocol_Plain"
}
```

### Development tips for email

When developing your app, you probably don't want to connect to a "live" mailserver that will send email to real-world external email acounts.
Instead, its nice to have a "dummy" server which will allow you to see the emails your application creates while making sure they aren't sent anywhere else.
You can set up your own server for this, but we recommend using [mailtrap.io](https://mailtrap.io), which offers such a service out of the box.
If you go there and sign in, you can get your own SMTP username and password for their `smtp.mailtrap.io`, and update `config/backend/email` accordingly.
