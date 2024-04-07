namespace MyFullStack01

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Templating

type MainTemplate = Template<"main.html", ClientLoad.FromDocument>

type Person =
    {
        FirstName: string
        LastName: string
        Age: int
    }

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/weather">] Weather of string
    | [<EndPoint "/square">] Square of int
    | [<EndPoint "POST /newperson"; Json "p">] SaveNewPerson of p:Person

module Site =
    open WebSharper.JavaScript
    open WebSharper.UI.Server

    open type WebSharper.UI.ClientServer

    module Server =
        [<Rpc>]
        let Echo msg =
            async {
                return sprintf "You sent: %s, I am sending OK." msg
            }
            
    module Client =
        open WebSharper.UI.Notation

        [<JavaScript>]
        let HomePage() =
            let v = Var.Create ""
            MainTemplate.ContactForm()
                .OnSend(fun e ->
                    async {
                        // Instead of reading e.Vars.Name.Value,
                        // get its value from its View asyncronously
                        let! name = e.Vars.Name.View |> View.GetAsync
                        let! email = e.Vars.Email.View |> View.GetAsync
                        let! msg = e.Vars.Message.View |> View.GetAsync
                        // Call the server
                        let! out = Server.Echo <| sprintf "From %s(%s): %s" name email msg
                        // Propagate the result to the UI via `v`
                        v := out
                    } |> Async.StartImmediate
                )
                // The response on the UI is tied to `v`
                .Response(v.View)
                .Doc()

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx -> function
            | EndPoint.Home ->
                Content.Page(
                    MainTemplate()
                        .Title("My WebSharper app")
                        .Container(client(Client.HomePage()))
                        .Doc()
                )
            | EndPoint.Weather city ->
                let weather = "sunny" // logic here
                Content.Text <| sprintf "The weather in %s is %s" city weather
            | EndPoint.Square f ->
                Content.Json (f*f)
            // You can test this endpoint with:
            // > curl --data '{"FirstName":"John", "LastName":"Smith", "Age":32}' \
            //   -k https://localhost:xxxxx/newperson
            //
            // where xxxxx is the port for your local deployment.
            | EndPoint.SaveNewPerson person ->
                Content.Json { person with LastName="Randall" }
        )

