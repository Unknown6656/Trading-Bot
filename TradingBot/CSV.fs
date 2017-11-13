module TradingBot.Csv

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection


type ColumnAttribute (index : int option, name : string option) =     
    inherit Attribute()     

    let mutable index = index
    let mutable name = name
    new () = ColumnAttribute (None, None)

    member __.Index          
        with get() = match index with
                     | Some i -> i
                     | None -> -1
        and set value = index <- Some value         
    member __.Name          
        with get() = match name with
                     | Some n -> n
                     | None -> ""
        and set value = name <- Some value     


type CsvReader<'a>(typeConverter:Type -> (string -> obj)) = 
    let mutable header = Map.empty
    let recordType = typeof<'a>  
    let fields = FSharpType.GetRecordFields(recordType)
    let objectBuilder = FSharpValue.PreComputeRecordConstructor(recordType)
    let split (delim:char) (line:string) = 
       [| delim |]
       |> line.Split
       |> Array.map (fun s -> s.Trim())

    member __.CreateRecord(header:Map<string,int>, delim, line) = 
        let lookupFromHeader (column:ColumnAttribute) = 
            match column.Name with
            | name when name <> String.Empty ->
                try
                    Some header.[name]
                with _ -> failwithf "no"
            | _ -> None
             
        let schema = fields
                     |> Array.mapi (fun fieldIndex field -> 
            let propertyInfo = recordType.GetProperty(field.Name)
            let deserializeColumnData = typeConverter field.PropertyType
            let columnIndex = 
                match propertyInfo.GetCustomAttributes(typeof<ColumnAttribute>,false) with
                | [| (:? ColumnAttribute as col) |] -> 
                    match col.Index with
                    | i when i >= 0 -> i 
                    | _ -> 
                        match lookupFromHeader col with
                        | Some(i) -> i
                        | None -> fieldIndex 
                | _ -> fieldIndex
                
            (fieldIndex, field.Name, columnIndex, deserializeColumnData) )
        
        let fieldContentFromSchema (words:string[]) = 
            Array.map(fun (_, _, columnIndex, deserializeColumnData) -> deserializeColumnData <| words.[columnIndex].Replace("\"", "")) schema

        let words = line
                    |> split delim
                    |> fieldContentFromSchema
        let convertColumn colText (fieldName, deserializeColumnData) =
            try deserializeColumnData colText
            with e ->
                failwithf "error converting '%s' to field '%s'" colText fieldName

        objectBuilder words
        |> unbox<'a>
    
    member x.ReadFile(file, separator:char, firstLineHasHeader:bool) = 
        seq { 
                use textReader = File.OpenText(file)
                if firstLineHasHeader then
                    header <- textReader.ReadLine() 
                              |> split separator
                              |> Array.filter (fun name -> not (String.IsNullOrWhiteSpace name))
                              |> Array.mapi (fun i name -> (name, i))
                              |> Map.ofArray
                while not textReader.EndOfStream do
                    let line = textReader.ReadLine()
                    if not (String.IsNullOrEmpty line) then
                        yield x.CreateRecord(header, separator, line)
            }
