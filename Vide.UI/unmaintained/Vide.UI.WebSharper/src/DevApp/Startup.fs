open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open WebSharper.AspNetCore

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    
    // Add services to the container.
    builder.Services.AddWebSharper()
        .AddAuthentication("WebSharper")
        .AddCookie("WebSharper", fun options -> ())
    |> ignore

    let app = builder.Build()

    // Configure the HTTP request pipeline.
    if not (app.Environment.IsDevelopment()) then
        app.UseExceptionHandler("/Error")
            // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
            .UseHsts()
        |> ignore
    
    app.UseHttpsRedirection()
        .UseDefaultFiles()
        .UseStaticFiles()
        //Enable if you want to make RPC calls to server
        //.UseWebSharperRemoting()
    |> ignore 
       
    app.Run()

    0 // Exit code
