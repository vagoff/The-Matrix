fun main () =
    s <- source 0;
    return <xml><body>
	<button value="inc" onclick={i <- get s; set s (i + 1)}/><br/>
	<button value="dec" onclick={i <- get s; set s (i - 1)}/><br/>
	value: {
	    i <- get s;
	    return <xml>{[i]}</xml>
	}
    </body></xml>
