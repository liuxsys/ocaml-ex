open Format


let init: int array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20|]

let rec sb (list: int array option) (num: int): int =
    
    let arrayabs: int array = 
        match list with 
            | Some x -> x
            | None  -> [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
    in 

   
    let absdiv: int = (Array.length arrayabs) / 2 in
    let base: int array = [|0|] in
    let res: int  =
      
        printf "Updates: %i \n" (Array.length arrayabs);
        
        if arrayabs.(absdiv) > num then
            
            let newarr: int array ref = ref [||]
            in
            let numm: int = ((Array.get arrayabs absdiv)) - 1 in
            
            for i = 0 to numm do
                base.(0) <- arrayabs.(i);
                let progresivearray: int array = Array.append !newarr base  in
                newarr := progresivearray;

            done;
            
            let num = sb (Some !newarr) num in
                        num
        else if arrayabs.(absdiv) = num then 
        (arrayabs.(absdiv))

        else 
            let newarr: int array ref = ref [||] 
            in
            for i = (arrayabs.(absdiv) - 1) to (Array.length arrayabs) - 1 do
                
                base.(0) <- arrayabs.(i);
                (*printf "base %i\n  cd %i\n length %i\n arrayabs: %i\n" base.(0) absdiv (Array.length arrayabs) arrayabs.(i);*)
                let progresivearray: int array = Array.append !newarr base  in
                newarr := progresivearray;
                (*Array.set newarr !d arrayabs.(i);*)
            done;
            let num = sb (Some !newarr) num in num

    in

    res;;

let exec: int = sb (Some init) 10;;

let () =
    printf "Response: %i" exec
    ;;