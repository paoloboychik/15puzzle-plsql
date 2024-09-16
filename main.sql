CREATE OR REPLACE PACKAGE BODY ind4 IS

/*PRIVATE*/

    /*custom datatypes*/
    TYPE arr1d_moves_type IS TABLE OF PLS_INTEGER;
    TYPE arr1d_vc_type IS TABLE OF VARCHAR(128) INDEX BY VARCHAR(128);
    TYPE arr1d_mask_type IS TABLE OF BOOLEAN INDEX BY VARCHAR(3);
    TYPE arr1d_sol_type IS TABLE OF VARCHAR(128) INDEX BY PLS_INTEGER;
    
    /*file info*/
    v_dir VARCHAR2(50);
    v_file_name VARCHAR2(50);
    
    /*N*/
    v_n NUMBER;
    
    /*arrays*/
    v_mtrx pr_q.arr2d_type;
    v_moves arr1d_moves_type := arr1d_moves_type(-1, 0, 1, 0, -1);
    v_visited arr1d_vc_type;
    
    /*exceptions*/
    e_no_solution EXCEPTION;
    e_incorrect_data EXCEPTION;
    
    /*create str-matrix*/
    FUNCTION to_str(p_mtrx IN pr_q.arr2d_type) RETURN VARCHAR2 IS
            v_str VARCHAR2(128);
        BEGIN
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    v_str := v_str || TO_CHAR(p_mtrx(i)(j))|| ',';
                END LOOP;
            END LOOP;
            RETURN v_str;
        END to_str;
    
    /*check matrix values*/
    FUNCTION check_mtrx_values(p_mtrx IN pr_q.arr2d_type) RETURN BOOLEAN IS
            v_mtrx_mask arr1d_mask_type;
            v_if_null_exists_already BOOLEAN := FALSE;
            v_if_all_values BOOLEAN := TRUE;
            v_str VARCHAR(3);
        BEGIN
            FOR i IN 1..POWER(v_n,2)-1 LOOP
                v_mtrx_mask(TO_CHAR(i)) := FALSE;
            END LOOP;
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    IF p_mtrx(i)(j) IS NULL THEN
                        IF v_if_null_exists_already THEN
                            DBMS_OUTPUT.PUT_LINE('More then one NULL in table!!!');
                            RETURN FALSE;
                        ELSE 
                            v_if_null_exists_already := TRUE;
                        END IF;
                    ELSIF p_mtrx(i)(j) BETWEEN 1 AND POWER(v_n,2)-1 THEN
                        v_str := TO_CHAR(p_mtrx(i)(j));
                        IF v_mtrx_mask.EXISTS(v_str) THEN
                            IF v_mtrx_mask(v_str) = FALSE THEN
                                v_mtrx_mask(TO_CHAR(p_mtrx(i)(j))) := TRUE;
                            ELSE
                                DBMS_OUTPUT.PUT_LINE('Values in table are repeated!!!');
                                RETURN FALSE;
                            END IF;                        
                        END IF;
                    ELSE
                        DBMS_OUTPUT.PUT_LINE('Values are out of range!!!');
                        RETURN FALSE;
                    END IF;
                END LOOP;
            END LOOP;
            FOR i IN 1..v_n-1 LOOP
                v_if_all_values := v_if_all_values AND v_mtrx_mask(TO_CHAR(i));
            END LOOP;
            IF NOT v_if_all_values THEN
                DBMS_OUTPUT.PUT_LINE('Not all values in table!!!');
                RETURN FALSE;
            END IF;
            RETURN TRUE;
        END check_mtrx_values;
    
    FUNCTION check_mtrx_solvability(p_mtrx IN pr_q.arr2d_type) RETURN BOOLEAN IS
            v_inversions NUMBER := 0;
            v_cur_i NUMBER;
        BEGIN
            FOR i IN 0..POWER(v_n,2)-1 LOOP
                FOR j IN i+1..POWER(v_n,2)-1 LOOP
                    IF p_mtrx(FLOOR(i/v_n)+1)(MOD(i,v_n)+1) > p_mtrx(FLOOR(j/v_n)+1)(MOD(j,v_n)+1) THEN
                        v_inversions := v_inversions + 1;
                    END IF;
                END LOOP;
            END LOOP;
            IF MOD(v_n, 2) = 1 THEN
                IF MOD(v_inversions, 2) = 1 THEN
                    RETURN FALSE;
                ELSE
                    RETURN TRUE;
                END IF;
            ELSE
                FOR i IN 0..POWER(v_n,2)-1 LOOP
                    IF p_mtrx(FLOOR(i/v_n)+1)(MOD(i,v_n)+1) IS NULL THEN
                        v_cur_i := v_n - FLOOR(i/v_n);
                        EXIT;
                    END IF;
                END LOOP;
                IF MOD(v_inversions, 2) + MOD(v_cur_i, 2) = 1 THEN
                    RETURN TRUE;
                ELSE
                    RETURN FALSE;
                END IF;
            END IF;
        END check_mtrx_solvability;
        
    /*check whether the target state is*/
    FUNCTION is_goal_state(p_mtrx IN pr_q.arr2d_type) RETURN BOOLEAN IS
        BEGIN
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    IF p_mtrx(i)(j) IS NULL AND (i-1)*v_n+j != POWER(v_n,2) OR p_mtrx(i)(j) != (i-1)*v_n+j THEN
                        RETURN FALSE;
                    END IF;
                END LOOP;
            END LOOP;
            RETURN TRUE;
        END is_goal_state;
    
    /*find empty cell*/
    FUNCTION find_empty(p_mtrx IN pr_q.arr2d_type) RETURN PLS_INTEGER IS
            v_temp PLS_INTEGER := POWER(v_n,2);
        BEGIN
            IF p_mtrx(v_n)(v_n) IS NULL THEN
                RETURN v_temp;
            END IF;
            FOR i IN 0..v_temp-2 LOOP
                IF p_mtrx(FLOOR(i/v_n)+1)(MOD(i,v_n)+1) IS NULL THEN
                    RETURN i+1;
                END IF;
            END LOOP;
            RETURN v_temp;
        END find_empty;
    
    /*manhattan distance*/
    FUNCTION manhattan_dist(p_mtrx IN pr_q.arr2d_type) RETURN PLS_INTEGER IS
            v_val PLS_INTEGER;
            v_i PLS_INTEGER;
            v_j PLS_INTEGER;
            v_res PLS_INTEGER := 0;
        BEGIN
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    v_val := p_mtrx(i)(j);
                    CONTINUE WHEN v_val IS NULL;
                    v_i := FLOOR((v_val - 1) / v_n) + 1;
                    v_j := MOD(v_val - 1, v_n) + 1;
                    v_res := v_res + ABS(v_i - i) + ABS(v_j - j);
                END LOOP;
            END LOOP;
            RETURN v_res;
        END manhattan_dist;
    
    /*swap 2 cells*/
    PROCEDURE swap(p_mtrx IN OUT pr_q.arr2d_type, p_i1 IN PLS_INTEGER, 
            p_j1 IN PLS_INTEGER, p_i2 IN PLS_INTEGER, p_j2 IN PLS_INTEGER) IS
        v_temp PLS_INTEGER;
        BEGIN
            v_temp := p_mtrx(p_i1)(p_j1);
            p_mtrx(p_i1)(p_j1) := p_mtrx(p_i2)(p_j2);
            p_mtrx(p_i2)(p_j2) := v_temp;
        END swap;
    
    /*read csv-matrix from file into v_mtrx*/
    PROCEDURE reading IS
            v_f UTL_FILE.FILE_TYPE;
            v_cur_str VARCHAR(128);
            v_tmp VARCHAR(128);
            v_tmp_mtrx pr_q.arr2d_type;
        BEGIN
            v_mtrx.DELETE;
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'r');
            UTL_FILE.GET_LINE(v_f, v_cur_str);
            FOR i IN 0..POWER(v_n,2)-1 LOOP
                v_tmp := REGEXP_SUBSTR(v_cur_str, '\d+|,\s*,|,\s*;', 1, i+1);
                v_tmp_mtrx(FLOOR(i/v_n)+1)(MOD(i,v_n)+1) := 
                    CASE WHEN REGEXP_LIKE(v_tmp, ',\s*,|,\s*;') THEN NULL ELSE TO_NUMBER(v_tmp) END;
            END LOOP;
            IF check_mtrx_values(v_tmp_mtrx) THEN
                IF check_mtrx_solvability(v_tmp_mtrx) THEN
                    v_mtrx := v_tmp_mtrx;
                ELSE
                    RAISE e_no_solution;
                END IF;
            ELSE
                RAISE e_incorrect_data;
            END IF;
        END reading;

    /*append current state of matrix to file*/
    PROCEDURE add_to_file(p_mtrx IN pr_q.arr2d_type) IS
            v_f UTL_FILE.FILE_TYPE;
            v_pad NUMBER := LENGTH(TO_CHAR(POWER(v_n, 2))) + 1;
        BEGIN
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'a');
            UTL_FILE.NEW_LINE(v_f);
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    UTL_FILE.PUT(v_f, RPAD(NVL(TO_CHAR(p_mtrx(i)(j)), ' '), v_pad));
                END LOOP;
                UTL_FILE.NEW_LINE(v_f);
            END LOOP;
            UTL_FILE.FCLOSE(v_f);
        END add_to_file;
        
    /*append str-matrix to file*/
    PROCEDURE add_to_file_str(p_str IN VARCHAR) IS
            v_f UTL_FILE.FILE_TYPE;
            v_pad NUMBER := LENGTH(TO_CHAR(POWER(v_n, 2))) + 1;
        BEGIN
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'a');
            UTL_FILE.NEW_LINE(v_f);
            FOR i IN 0..POWER(v_n,2)-1 LOOP
                UTL_FILE.PUT(v_f, RPAD(REGEXP_REPLACE(REGEXP_SUBSTR(p_str, '\d+|,,|,;', 1, i+1), ',,|,;', ' '), v_pad));
                IF FLOOR(i/v_n)+1 < FLOOR((i+1)/v_n)+1 THEN
                    UTL_FILE.NEW_LINE(v_f);
                END IF;
            END LOOP;
            UTL_FILE.FCLOSE(v_f);
        END add_to_file_str;

    /*output current matrix*/
    PROCEDURE output_mtrx(p_mtrx IN pr_q.arr2d_type) IS
            v_pad NUMBER := LENGTH(TO_CHAR(POWER(v_n, 2))) + 1;
        BEGIN
            FOR i IN 1..v_n LOOP
                FOR j IN 1..v_n LOOP
                    DBMS_OUTPUT.PUT(RPAD(NVL(TO_CHAR(p_mtrx(i)(j)), ' '), v_pad));
                END LOOP;
                DBMS_OUTPUT.NEW_LINE;
            END LOOP;
        END output_mtrx;
    
    /*output current str-matrix*/
    PROCEDURE output_mtrx_str(p_str IN VARCHAR) IS
            v_pad NUMBER := LENGTH(TO_CHAR(POWER(v_n, 2))) + 1;
        BEGIN
            FOR i IN 0..POWER(v_n,2)-1 LOOP
                DBMS_OUTPUT.PUT(RPAD(REGEXP_REPLACE(REGEXP_SUBSTR(p_str, '\d+|,,|,;', 1, i+1), ',,|,;', ' '), v_pad));
                IF FLOOR(i/v_n)+1 < FLOOR((i+1)/v_n)+1 THEN
                    DBMS_OUTPUT.NEW_LINE;
                END IF;
            END LOOP;
        END output_mtrx_str;
    
    /*find solution by Greedy (or A*)*/
    PROCEDURE greedy(p_mtrx IN pr_q.arr2d_type) IS
            v_queue pr_q.priority_queue;
            v_cur_state pr_q.priority_arr2d_type;
            v_next_i PLS_INTEGER;
            v_next_j PLS_INTEGER;
            v_empty PLS_INTEGER := find_empty(p_mtrx);
            v_next_mtrx pr_q.arr2d_type;
            v_next_depth PLS_INTEGER;
            v_next_priority PLS_INTEGER;
            v_str VARCHAR(128);
            v_solution_list arr1d_sol_type;
            v_f UTL_FILE.FILE_TYPE;
        BEGIN
            v_visited.DELETE;
            pr_q.push(v_queue, p_mtrx, FLOOR((v_empty-1)/v_n)+1, MOD(v_empty-1,v_n)+1, 0, 0);
            v_visited(to_str(p_mtrx)) := '';
            
            WHILE v_queue.COUNT > 0 LOOP
                v_cur_state := pr_q.pop(v_queue);
                IF is_goal_state(v_cur_state.arr) THEN
                    DBMS_OUTPUT.PUT_LINE('Solution was founded!');
                    DBMS_OUTPUT.PUT_LINE('(with depth: ' || TO_CHAR(v_cur_state.depth) || ')');
                    v_str := to_str(v_cur_state.arr);
                    v_solution_list(v_cur_state.depth) := v_str;
                    WHILE v_visited(v_str) IS NOT NULL LOOP
                        v_solution_list(v_solution_list.FIRST-1) := v_visited(v_str);
                        v_str := v_visited(v_str);
                    END LOOP;
                    FOR i IN v_solution_list.FIRST+1..v_solution_list.LAST LOOP
                        add_to_file_str(v_solution_list(i));
                    END LOOP;
                    RETURN;
                END IF;
                
                FOR i IN 1..4 LOOP
                    v_next_i := v_cur_state.i_empty + v_moves(i);
                    v_next_j := v_cur_state.j_empty + v_moves(i+1);
                    
                    CONTINUE WHEN (v_next_i NOT BETWEEN 1 AND v_n) OR (v_next_j NOT BETWEEN 1 AND v_n);
                    
                    v_next_mtrx := v_cur_state.arr;
                    swap(v_next_mtrx, v_cur_state.i_empty, v_cur_state.j_empty, v_next_i, v_next_j);
                    
                    CONTINUE WHEN v_visited.EXISTS(to_str(v_next_mtrx));
                    
                    v_visited(to_str(v_next_mtrx)) := to_str(v_cur_state.arr);
                    
                    v_next_depth := v_cur_state.depth+1;
                    
                    --uncomment if A*
                    v_next_priority := /*v_next_depth + */manhattan_dist(v_next_mtrx);
                    pr_q.push(v_queue, v_next_mtrx, v_next_i, v_next_j, v_next_depth, v_next_priority);                    
                END LOOP;
            
            END LOOP;
            RETURN;
        END greedy;

/*PUBLIC*/

    /*input*/
    PROCEDURE input_file(p_n IN NUMBER, p_csv_str IN VARCHAR2, 
        p_file_name IN VARCHAR2 DEFAULT 'ind4_kononovpo.txt', p_dir IN VARCHAR2 DEFAULT 'STUD_PLSQL') IS
            v_f UTL_FILE.FILE_TYPE;
            v_cur_str VARCHAR2(128);
        BEGIN
            v_n := p_n;
            v_file_name := p_file_name;
            v_dir := p_dir;
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'w');
            v_cur_str := REGEXP_SUBSTR(p_csv_str, '(\s*\d*\s*(,|;)){'||TO_CHAR(POWER(v_n,2)-1)||'}\s*\d*\s*;', 1, 1);
            IF v_cur_str IS NULL THEN
                RAISE e_incorrect_data;
            ELSE
                UTL_FILE.PUT_LINE(v_f, v_cur_str);
            END IF;
            UTL_FILE.FCLOSE(v_f);
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'r');
            LOOP
                BEGIN
                    UTL_FILE.GET_LINE(v_f, v_cur_str);
                EXCEPTION 
                    WHEN NO_DATA_FOUND THEN EXIT;
                END;
            END LOOP;
            UTL_FILE.FCLOSE(v_f);
            DBMS_OUTPUT.PUT_LINE('User data has been read successfully!');
        EXCEPTION
            WHEN e_incorrect_data THEN
               DBMS_OUTPUT.PUT_LINE('Incorrect user data!!!'); 
        END input_file;

    /*line by line output*/
    PROCEDURE output_file IS
            v_f UTL_FILE.FILE_TYPE;
            v_cur_str VARCHAR(128);
            v_pad NUMBER := LENGTH(TO_CHAR(POWER(v_n, 2))) + 1;
            v_step NUMBER := 1;
        BEGIN
            v_f := UTL_FILE.FOPEN(v_dir, v_file_name, 'r');
            LOOP
                BEGIN
                    UTL_FILE.GET_LINE(v_f, v_cur_str);
                    IF v_cur_str IS NULL THEN
                        DBMS_OUTPUT.NEW_LINE;
                        DBMS_OUTPUT.PUT_LINE(TO_CHAR(v_step) || ' step:');
                        v_step := v_step + 1;
                    ELSIF REGEXP_COUNT(v_cur_str, ',') > 0 THEN
                        output_mtrx_str(v_cur_str);
                    ELSE
                        DBMS_OUTPUT.PUT_LINE(v_cur_str);
                    END IF;
                EXCEPTION 
                    WHEN NO_DATA_FOUND THEN EXIT;
                END;
            END LOOP;
            UTL_FILE.FCLOSE(v_f);
        END output_file;
        
        /*solution*/
    PROCEDURE solution IS 
        BEGIN
            reading; 
            greedy(v_mtrx); 
            UTL_FILE.FCLOSE_ALL;
        EXCEPTION
            WHEN e_incorrect_data THEN
                DBMS_OUTPUT.PUT_LINE('Incorrect input data!!!');
            WHEN e_no_solution THEN
                DBMS_OUTPUT.PUT_LINE('No solution!!!');
        END solution;

END ind4;


/


CREATE OR REPLACE PACKAGE ind4 IS
    
    PROCEDURE input_file(p_n IN NUMBER, p_csv_str IN VARCHAR2, 
        p_file_name IN VARCHAR2 DEFAULT 'ind4_kononovpo.txt', p_dir IN VARCHAR2 DEFAULT 'STUD_PLSQL');
    PROCEDURE output_file;
    PROCEDURE solution;
END ind4;


/


SET SERVEROUTPUT ON;

EXEC ind4.output_file;
EXEC ind4.solution;

CALL ind4.input_file( 
    p_n => 2,
    p_csv_str => 
           '2,3;'
        || '1,;'
);
CALL ind4.input_file( 
    p_n => 3,
    p_csv_str => 
           '8,6,7;'
        || '2,5,4;'
        || '3,,1;'
);
CALL ind4.input_file( 
    p_n => 4,
    p_csv_str => 
           '7,4,3,5;'
        || '14,9,8,13;'
        || '1,11,6,12;'
        || '15,10,2,;'
);
CALL ind4.input_file( 
    p_n => 5,
    p_csv_str => 
           '6,13,22,20,8;'
        || '23,12,24,4,7;'
        || '14,15,11,9,5;'
        || '1,19,18,17,16;'
        || '3,10,2,21,;'
);
CALL ind4.input_file( 
    p_n => 6,
    p_csv_str => 
           '7,1,2,3,4,5;'
        || '13,14,8,9,10,6;'
        || '19,20,16,22,11,12;'
        || '25,26,15,21,17,18;'
        || '31,27,28,29,23,24;'
        || '32,33,34,35,30,;'
);
