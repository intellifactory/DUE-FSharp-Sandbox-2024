namespace MyFullStack01

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Templating

type MainTemplate = Template<"main.html", ClientLoad.FromDocument>

[<AutoOpen;JavaScript>]
module Extensions =
    type AsyncBuilder with
        // let!
        member this.Bind(var: Var<'T>, continuation: 'T -> Async<'U>) : Async<'U> =
            var.View |> View.GetAsync |> fun res -> async.Bind(res, continuation)
        member this.Bind(view: View<'T>, continuation: 'T -> Async<'U>) : Async<'U> =
            view |> View.GetAsync |> fun res -> async.Bind(res, continuation)

    type Var<'T> with
        member this.GetAsync =
            this.View |> View.GetAsync

type Person =
    {
        FirstName: string
        LastName: string
        Age: int
    }

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/calc">] Calculator
    | [<EndPoint "/forms">] Forms
    | [<EndPoint "/charting">] Charting
    | [<EndPoint "/weather">] Weather of string
    | [<EndPoint "/square">] Square of int
    | [<EndPoint "POST /newperson"; Json "p">] SaveNewPerson of p:Person

module Server =
    [<Rpc>]
    let Echo msg =
        async {
            return sprintf "You sent: %s, I am sending OK." msg
        }
            
[<JavaScript>]
module Client =
    open WebSharper.JavaScript
    open WebSharper.UI.Notation
    open WebSharper.Charting
    open WebSharper.Forms

    let HomePage() =
        if IsClient then
            let v = Var.Create ""
            MainTemplate.ContactForm()
                .OnSend(fun e ->
                    async {
                        // Instead of reading e.Vars.Name.Value,
                        // get its value from its View asyncronously
                        let! name = e.Vars.Name
                        // Check if name is empty, ...
                        if name = "" then
                            () // ...
                        else
                            let! email = e.Vars.Email.View
                            let! msg = e.Vars.Message
                            // Call the server
                            let! out = Server.Echo <| sprintf "From %s(%s): %s" name email msg
                            // Propagate the result to the UI via `v`
                            v := out
                    } |> Async.StartImmediate
                )
                // The response on the UI is tied to `v`
                .Response(v.View)
                .Doc()
        else
            MainTemplate.ContactForm()
                .Doc()

    open Calculator
    open WebSharper.JavaScript

    let line(ctx: CanvasRenderingContext2D, x1, y1, x2: float, y2: float) =
        ctx.BeginPath()
        ctx.MoveTo(x1, y1)
        ctx.LineTo(x2, y2)
        ctx.Stroke()

    let evalAt e x =
        let env = ["x", x]
        Evaluator.Eval env e

    let Calculator () =
        Form.Return (fun v from ``to`` formula -> v, from, ``to``, formula)
        <*> (Form.Yield "x" |> Validation.IsNotEmpty "Variable should be non-empty")
        <*> (Form.Yield "-20" |> Validation.IsMatch "[0-9]+" "From should be a number")
        <*> (Form.Yield "20" |> Validation.IsMatch "[0-9]+" "To should be a number")
        <*> (Form.Yield "sin(x*x)*cos(x)" |> Validation.IsNotEmpty "Formula should be non-empty")
        |> Form.WithSubmit
        |> Form.Render (fun v from ``to`` formula submitter ->
            MainTemplate.Calculator()
                .Variable(v)
                .From(from)
                .To(``to``)
                .Formula(formula)
                .OnSend(fun node ->
                    async {
                        let! formula = node.Vars.Formula
                        let! from = node.Vars.From
                        let! ``to`` = node.Vars.To
                        match formula with
                        | Language.Expression (e, Language.Eof) ->
                            let scaleX = 50.
                            let scaleY = 50.
                            let from = float from
                            let ``to`` = float ``to``
                            let ctx = As<HTMLCanvasElement>(node.Anchors.Canvas).GetContext "2d"
                            ctx.ClearRect(0, 0, 1000, 1000)
                            let skip = (``to`` - from) / 1000.
                            [from .. skip .. ``to``]
                            |> Seq.fold (fun (x1,y1) x2 ->
                                let y2 = evalAt e x2
                                line(ctx, 
                                    x1*scaleX+500., y1*scaleY+200.,
                                    x2*scaleX+500., y2*scaleY+200.)
                                (x2, y2)
                            ) (from, evalAt e from)
                            |> ignore
                        | _ ->
                            failwithf "Syntax error in formula, expecting an expression"
                    } |> Async.StartImmediate
                )
                .Doc()
        )

    let ContactForm() =
        let v = Var.Create ""
        Form.Return (fun name email message -> name, email, message)
        <*> (Form.Yield "" |> Validation.IsNotEmpty "Name should be non-empty")
        <*> Form.Yield ""
        <*> Form.Yield ""
        |> Form.WithSubmit
        |> Form.Run (fun (name, email, message) ->
            async {
                // Call the server
                let! out = Server.Echo <| sprintf "From %s(%s): %s" name email message
                // Propagate the result to the UI via `v`
                v := out
            } |> Async.StartImmediate
        )
        |> Form.Render (fun name email message submitter ->
            MainTemplate.ContactForm()
                .Name(name)
                .Email(email)
                .Message(message)
                .Response(v.View)
                .OnSend(fun e -> submitter.Trigger())
                .Doc()
        )

    let Charts() =
        let labels =
            [| "Eating"; "Drinking"; "Sleeping";
               "Designing"; "Coding"; "Cycling"; "Running" |]
        let dataset1 = [|28.0; 48.0; 40.0; 19.0; 96.0; 27.0; 100.0|]
        let dataset2 = [|65.0; 59.0; 90.0; 81.0; 56.0; 55.0; 40.0|]
    
        let chart =
            Chart.Combine [
                Chart.Radar(Array.zip labels dataset1)
                    .WithFillColor(Color.Rgba(151, 187, 205, 0.2))
                    .WithStrokeColor(Color.Name "blue")
                    .WithPointColor(Color.Name "darkblue")
                    .WithTitle("Alice")

                Chart.Radar(Array.zip labels dataset2)
                    .WithFillColor(Color.Rgba(220, 220, 220, 0.2))
                    .WithStrokeColor(Color.Name "green")
                    .WithPointColor(Color.Name "darkgreen")
                    .WithTitle("Bob")
            ]
        Renderers.ChartJs.Render(chart, Size = Size(500, 300))

module Site =
    open WebSharper.JavaScript
    open WebSharper.UI.Server
    open WebSharper.UI.Html

    open type WebSharper.UI.ClientServer

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx -> function
            | EndPoint.Home ->
                Content.Page(
                    MainTemplate()
                        .Title("My WebSharper app")
                        .Container(hydrate(Client.HomePage()))
                        .Doc()
                )
            | EndPoint.Calculator ->
                Content.Page([
                    MainTemplate()
                        .Title("My WebSharper calculator")
                        .Container(client(Client.Calculator()))
                        .Doc()
                ])
            | EndPoint.Charting ->
                Content.Page([
                    // No templating would work, because we are not serving
                    // the inner templates from main.html
                    client(Client.Charts())
                ])
            | EndPoint.Forms ->
                Content.Page([
                    MainTemplate()
                        .Title("My WebSharper app")
                        .Container(client(Client.ContactForm()))
                        .Doc()
                ])
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

