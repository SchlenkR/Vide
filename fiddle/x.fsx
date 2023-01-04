#r "nuget: FsHttp"

open System
open FsHttp

http {
    GET "https://www.google.de"
    AuthorizationBearer "YoutToken"
    config_update (fun config ->
        { config with timeout = Some (TimeSpan.FromSeconds 10.0)})
}
