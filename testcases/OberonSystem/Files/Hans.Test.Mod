MODULE Test;
	IMPORT Out;
	
	VAR A,B: ARRAY 32 OF CHAR;
	i: INTEGER;

BEGIN
	B := "this is a string";
	A[0] := B[3];
	FOR i := 0 TO 6 DO
		A[i] := B[i];
	END;
	Out.String(A); 
	Out.Ln();
END Test.
