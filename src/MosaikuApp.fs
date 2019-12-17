module MosaikuApp

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open System
// open Fable.Import.Browser

// // HELPER

// let (?) (this : 'Source) (prop : string) : 'Result =
//   let p = this.GetType().GetProperty(prop)
//   p.GetValue(this, null) :?> 'Result

// MODEL

type Position (x:int, y:int) =
  member __.X = x
  member __.Y = y

  static member (+) (pos1:Position, pos2:Position) =
    Position(pos1.X + pos2.X, pos1.Y + pos2.Y)

  override this.ToString() =
    this.X.ToString() + "," + this.Y.ToString()

  // IComparable IEquatable
  member private this.Ident = this.X, this.Y

  // interface IComparable<Position> with
  //   member this.CompareTo other =
  //     compare this.Ident other.Ident

  interface IComparable with
    member this.CompareTo obj =
      match obj with
        | null -> 1
        // | :? Position as other -> (this :> IComparable<_>).CompareTo other
        | :? Position as other -> compare this.Ident other.Ident
        | _ -> invalidArg "obj" "not a Position"

  interface IEquatable<Position> with
    member this.Equals other =
      this.Ident = other.Ident
          
  override this.Equals obj =
    match obj with
    | :? Position as other -> this.Ident = other.Ident
    // | :? Position as other -> (this :> IEquatable<_>).Equals other
    | _ -> false
  override this.GetHashCode() =
    hash this.Ident

// From my point of view, Mosaiku are just buggy deminor so:
type CellState =
  | Unknown
  | Bomb
  | Safe

type ValidationState =
  | Valid
  | Invalid

type CellData = int option * CellState * ValidationState

type Table = Map<Position, CellData>

type Model = {
  HasUpdate : bool
  Height : int
  Width : int
  Active : Position option
  Cells : Table
  }

type Msg =
| OnOffUpdate
| StartEdit of Position
| UpdateValue of Position * int
| SetBomb
| SetSafe
| SetUnknown
| ChangeState of Position
| SearchSolution

let inputData =
  [
    -1;-1;-1;2;1;0;-1;-1;-1;0;2;-1;2;-1;-1;-1;0;
    -1;5;-1;5;4;-1;1;-1;-1;-1;-1;-1;-1;1;-1;-1;-1;
    1;-1;5;-1;-1;-1;4;-1;1;-1;-1;4;-1;-1;1;-1;-1;
    -1;-1;2;3;-1;-1;-1;-1;4;2;2;-1;-1;5;-1;1;-1;
    -1;-1;1;-1;-1;-1;4;4;-1;-1;4;-1;4;-1;-1;-1;1;
    -1;-1;-1;-1;-1;-1;-1;-1;-1;4;-1;-1;-1;-1;5;-1;-1;
    -1;-1;-1;-1;6;-1;-1;-1;3;-1;4;-1;5;-1;6;-1;-1;
    -1;-1;2;-1;5;-1;-1;-1;-1;3;-1;3;-1;-1;6;-1;6;
    -1;-1;-1;-1;-1;3;-1;4;-1;-1;-1;2;2;4;-1;6;-1;
    3;-1;4;-1;5;-1;3;-1;-1;-1;-1;4;-1;2;2;3;-1;
    -1;5;-1;-1;-1;-1;5;3;-1;3;-1;-1;-1;4;-1;1;-1;
    3;-1;-1;3;-1;4;-1;-1;-1;-1;2;4;-1;-1;-1;4;-1;
    -1;5;4;-1;-1;1;2;-1;-1;-1;1;-1;-1;4;-1;-1;2;
    -1;-1;5;4;-1;1;-1;-1;2;-1;-1;-1;-1;-1;-1;4;-1;
    2;-1;-1;5;-1;3;-1;1;-1;5;-1;1;-1;-1;2;-1;3;
    -1;3;2;3;-1;-1;4;-1;-1;-1;6;-1;1;-1;-1;-1;4;
    4;-1;-1;-1;3;-1;-1;-1;-1;7;-1;-1;3;1;2;-1;5;
    -1;6;-1;3;-1;3;-1;6;-1;-1;4;-1;-1;4;-1;-1;-1;
    -1;5;7;-1;4;-1;-1;3;-1;1;-1;3;-1;-1;-1;-1;3;
    1;4;6;-1;-1;-1;4;-1;1;-1;-1;-1;2;3;4;-1;2;
    -1;-1;-1;-1;5;-1;5;-1;-1;-1;-1;-1;-1;-1;2;-1;3;
    -1;-1;-1;5;-1;-1;-1;-1;5;-1;-1;-1;-1;-1;-1;4;-1;
    -1;3;6;6;3;-1;1;-1;-1;-1;3;1;-1;-1;2;-1;5;
    -1;3;-1;-1;-1;-1;-1;-1;3;5;-1;3;1;1;3;-1;-1;
    -1;-1;-1;-1;-1;-1;-1;-1;1;-1;5;-1;-1;4;-1;-1;3;
    -1;-1;3;2;0;-1;-1;-1;-1;1;-1;4;4;-1;-1;3;1
  ]

let init() : Model = {
  HasUpdate = false
  Height = 26
  Width = 17
  Active = None
  Cells = inputData |> List.mapi (fun i x -> Position(i / 17,i % 17), ((if x = -1 then None else Some x), Unknown, Valid)) |> Map.ofList
}

let valueExpected (data:CellData) =
  let valueExpected, _, _ = data
  valueExpected

let cellState (data:CellData) =
  let _, cellState, _ = data
  cellState

let validationState (data:CellData) =
  let _, _, validationState = data
  validationState

let isValid pos expected model =
  let cellData = Map.tryFind pos model.Cells
  if cellData = None then
    true
  else
    let testedPos = Position(16,9)
    if pos = testedPos then eprintfn "%A - %A" model.Height model.Width
    let nbBomb, nbLeft =
      [-1..1]
      |> List.collect (fun deltaX ->
        [-1..1]
        |> List.map (fun deltaY ->
          let currPos = pos + Position(deltaX, deltaY)
          if currPos.X >= 0 && currPos.X < model.Height && currPos.Y >= 0 && currPos.Y < model.Width then
            let curCellData = Map.tryFind currPos model.Cells
            match curCellData with
            | Some curCellData ->
              match curCellData with
                | _, cellState, _ when cellState = Bomb ->
                  if pos = testedPos then eprintfn "%A - bomb" currPos
                  (1, 0)
                | _, cellState, _ when cellState = Safe ->
                  if pos = testedPos then eprintfn "%A - safe" currPos
                  (0, 0)
                | _ ->
                  if pos = testedPos then eprintfn "%A - other" currPos
                  (0, 1)
            | None ->
              if pos = testedPos then eprintfn "%A - nothing" currPos
              (0, 1)
          else
            if pos = testedPos then eprintfn "%A - else" currPos
            (0, 0)
        )
      )
      |> List.fold (fun acc curr -> ((fst acc) + (fst curr), (snd acc) + (snd curr))) (0,0)

    let expectBomb = (valueExpected (cellData.Value)).Value
    if pos = testedPos then eprintfn "exp - bomb - left: %A - %A - %A" expectBomb nbBomb nbLeft
    nbBomb <= expectBomb && (nbBomb + nbLeft) >= expectBomb

let updateCellWithActiveOrNot (pos, cell) model =
  let value, cellState, _ = cell
  let newState = if isValid pos ((valueExpected cell).Value) model then Valid else Invalid
  {model with Cells = model.Cells.Add(pos, (value, cellState, newState))}

let updateValidateModel (model:Model) =

  let rec updateValidateModelRec cells model =
    match cells with
      | [] -> model
      | head::tail -> updateValidateModelRec tail (updateCellWithActiveOrNot head model)

  let cellsToUpdate =
    model.Cells
    |> Map.toList
    |> List.filter (fun (pos, data) -> (valueExpected data).IsSome)
  updateValidateModelRec cellsToUpdate model

// UPDATE

let updateState (model:Model) pos newState =
  match pos with
    | Some pos ->
      let cellData = Map.tryFind pos model.Cells
      let cellValue =
        match cellData with
          | Some (value, _, _) -> value
          | _ -> None
      updateValidateModel { model with Cells = model.Cells.Add(pos, (cellValue, newState, Valid)) }
    | _ ->
      eprintfn "No active cell to update."
      model

let retrieveNextState pos model =
  let cellData = Map.tryFind pos model.Cells
  match cellData with
    | Some (_, state, _) ->
      match state with
        | Bomb -> Safe
        | Safe -> Unknown
        | Unknown -> Bomb
    | _ -> Bomb

let update (msg:Msg) (model:Model) =
    match msg with
    | OnOffUpdate -> { model with HasUpdate = model.HasUpdate = false }
    | StartEdit pos ->
      //eprintfn "StartEdit %A" pos
      { model with Active = Some pos }
    | UpdateValue (pos, value) ->
      let cellData = Map.tryFind pos model.Cells
      let cellState =
        match cellData with
          | Some (_, state, _) -> state
          | _ -> Unknown
      updateValidateModel { model with Cells = model.Cells.Add(pos, ((if value = -1 then None else Some value), cellState, Valid)) }
    | SetBomb -> updateState model model.Active Bomb
    | SetSafe -> updateState model model.Active Safe
    | SetUnknown -> updateState model model.Active Unknown
    | ChangeState pos -> updateState { model with Active = Some pos } model.Active (retrieveNextState pos model)
    | SearchSolution -> model

// VIEW (rendered with React)

let retrieveValue cellData =
  match cellData with
    | Some (numBomb, _, _) -> numBomb
    | _ -> None

let valueToDisplay cellData =
  match retrieveValue cellData with
    | Some value -> string value
    | _ -> ""

let styleToUse cellData =
  match cellData with
  | Some (_, state, valid) ->
    let backgroundColor =
      match state with
      | Bomb ->
        //eprintfn "a bomb"
        Background "#ffb0b0"
      | Safe ->
        //eprintfn "a safe"
        Background "#b0ffb0"
      | _ -> Background "white"
    let color =
      match valid with
      | Valid ->
        //eprintfn "a valid"
        Color "black"
      | Invalid ->
        //eprintfn "a invalid"
        Color "red"
    [ backgroundColor;
    color ]      
  | _ -> [ Background "white";
            Color "black"]

let renderEditor dispatch pos cellData = 
  td 
    [ Class "selected" ] 
    [ input [
        Style (styleToUse cellData)
        AutoFocus true
        OnChange (fun e -> dispatch(UpdateValue (pos, int e.Value)))
        //OnInput (fun e -> dispatch(UpdateValue (pos, int e.Value)))
        Value (valueToDisplay cellData) ] ]

let keyDownMapping dispatch (e:Browser.Types.KeyboardEvent) =
  //eprintfn "td-key: %A - %A" e.key e.keyCode
  match char e.keyCode with
  | 'b' | 'B' -> dispatch(SetBomb)
  | 's' | 'S' -> dispatch(SetSafe)
  | 'u' | 'U' -> dispatch(SetUnknown)
  | _ -> ()

let renderView dispatch pos cellData model = 
  td 
    [ Style (styleToUse cellData)
      OnClick (fun _ -> 
        //eprintfn "start edit"
        dispatch(StartEdit(pos)))
      OnDoubleClick (fun _ -> 
        //eprintfn "double clic"
        if model.HasUpdate then dispatch(StartEdit(pos)) else dispatch(ChangeState(pos)))
      OnKeyDown (keyDownMapping dispatch)
      TabIndex 0 ] 
    [ str (valueToDisplay cellData) ]

let renderCell dispatch pos model =
  let cellData = Map.tryFind pos model.Cells
  if model.Active = Some pos && model.HasUpdate then
    renderEditor dispatch pos cellData
  else
    renderView dispatch pos cellData model

let view (model:Model) dispatch =

  let cells r =
    [0..(model.Width - 1)]
    |> List.map (fun c -> renderCell dispatch (Position(r,c)) model )

  let rows =
    [0..(model.Height - 1)]
    |> List.map (fun r -> tr [] (cells r))

  // table []
  //   [ tbody [] rows ]
  div []
     [ div []
        [ button [ OnClick (fun _ -> dispatch OnOffUpdate) ] [ str "Update On/Off" ]
          button [ OnClick (fun _ -> dispatch SetBomb) ] [ str "Bomb! (b)" ]
          button [ OnClick (fun _ -> dispatch SetSafe) ] [ str "Safe! (s)" ]
          button [ OnClick (fun _ -> dispatch SetUnknown) ] [ str "Unknown (u)" ] ]
       //div [] [ str ("Jonathan") ] ]
       table []
          [ tbody [] rows ] ]

  // div []
  // [ div []
  //   [ button [ OnClick (fun _ -> dispatch SetBomb) ] [ str "Bomb!" ]
  //     button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]
  //   div []
  //   [ div [] [ str (string model) ]
  //     div [] [ str ("Jonatan") ] ] ]

// let inputs dispatch =
//   let update (e : KeyboardEvent, pressed) =
//       match e.key with
//       | "w" -> KeyUp |> Input |> dispatch
//       | "a" -> KeyLeft |> Input |> dispatch
//       | "s" -> KeyDown |> Input |> dispatch
//       | "d" -> KeyRight |> Input |> dispatch
//       | _ -> ()
//   document.addEventListener("keydown", fun e -> update(e :?> _, true))

// App
Program.mkSimple init update view
// |> Program.withSubscription (fun _ -> Cmd.ofSub inputs)
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
