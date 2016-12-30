# oauth2ems
Compatível com https://github.com/kivra/oauth2 (release 0.4.1) e https://github.com/erlangMS/ems-bus



oauth2ems_backend:start().

## URLs de teste

http://127.0.0.1:2301/authorize?grant_type=password&username=johndoe&password=A3ddj3w
http://127.0.0.1:2301/authorize?response_type=code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb
http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb&username=johndoe&password=A3ddj3w&secret=%22qwer%22
http://127.0.0.1:2301/authorize?response_type=token&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb
http://127.0.0.1:2301/authorize?grant_type=authorization_code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=https%3A%2F%2Fclient%2Eexample%2Ecom%2Fcb&username=johndoe&password=A3ddj3w&secret=qwer&code=5S8qX7DwgaBb7SW9F5S7lrxjphhTF92S
