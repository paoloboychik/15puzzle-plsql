CREATE OR REPLACE PACKAGE BODY priority_queue_package IS
    
    FUNCTION get_parent_id(p_index IN PLS_INTEGER) RETURN PLS_INTEGER IS 
        BEGIN   
            RETURN ROUND((p_index - 1) / 2);
        END get_parent_id;
        
    FUNCTION get_left_child_id(p_index In PLS_INTEGER) RETURN PLS_INTEGER IS 
        BEGIN   
            RETURN 2 * p_index;
        END get_left_child_id;
        
    FUNCTION get_right_child_id(p_index IN PLS_INTEGER) RETURN PLS_INTEGER IS 
        BEGIN 
            RETURN 2 * p_index + 1; 
        END get_right_child_id;
    
    PROCEDURE swap(p_priority_queue IN OUT priority_queue, p_idx1 IN PLS_INTEGER, p_idx2 IN PLS_INTEGER) IS
            v_tmp priority_arr2d_type;
        BEGIN
            v_tmp := p_priority_queue(p_idx1);
            p_priority_queue(p_idx1) := p_priority_queue(p_idx2);
            p_priority_queue(p_idx2) := v_tmp;
        END swap;
        
    PROCEDURE sift_up(p_priority_queue IN OUT priority_queue, p_idx IN PLS_INTEGER) IS
            v_parent_idx PLS_INTEGER := get_parent_id(p_idx);
            v_idx PLS_INTEGER := p_idx;
        BEGIN
            WHILE v_idx > 1 AND p_priority_queue(v_idx).priority < p_priority_queue(v_parent_idx).priority LOOP
                swap(p_priority_queue, v_idx, v_parent_idx);
                v_idx := v_parent_idx;
                v_parent_idx := get_parent_id(v_idx);
            END LOOP;
        END sift_up;
    
    PROCEDURE sift_down(p_priority_queue IN OUT priority_queue, p_idx PLS_INTEGER) IS
            v_left_child_idx PLS_INTEGER;
            v_right_child_idx PLS_INTEGER;
            v_max PLS_INTEGER;
            v_idx PLS_INTEGER := p_idx;
        BEGIN
            WHILE 1 = 1 LOOP
                v_max := v_idx;
                v_left_child_idx := get_left_child_id(v_idx);
                v_right_child_idx := get_right_child_id(v_idx);
                IF v_left_child_idx <= p_priority_queue.COUNT 
                    AND p_priority_queue(v_left_child_idx).priority < p_priority_queue(v_max).priority THEN
                    v_max := v_left_child_idx;
                END IF;
                IF v_right_child_idx <= p_priority_queue.COUNT 
                    AND p_priority_queue(v_right_child_idx).priority < p_priority_queue(v_max).priority THEN
                    v_max := v_right_child_idx;
                END IF;
                IF v_max = v_idx THEN
                    EXIT;
                END IF;
                swap(p_priority_queue, v_idx, v_max);
                v_idx := v_max;
            END LOOP;
        END sift_down;
    
    PROCEDURE push(p_priority_queue IN OUT priority_queue, p_arr IN arr2d_type, p_i_empty PLS_INTEGER, 
        p_j_empty PLS_INTEGER, p_depth IN PLS_INTEGER, p_priority IN PLS_INTEGER) IS
            v_index PLS_INTEGER;
        BEGIN
            v_index := p_priority_queue.COUNT + 1;
            p_priority_queue(v_index) := NULL;
            p_priority_queue(v_index).arr := p_arr;
            p_priority_queue(v_index).i_empty := p_i_empty;
            p_priority_queue(v_index).j_empty := p_j_empty;
            p_priority_queue(v_index).depth := p_depth;
            p_priority_queue(v_index).priority := p_priority;
            sift_up(p_priority_queue, v_index);
        END push;
    
    FUNCTION pop(p_priority_queue IN OUT priority_queue) RETURN priority_arr2d_type IS
            v_tmp priority_arr2d_type;
        BEGIN
            v_tmp := p_priority_queue(1);
            swap(p_priority_queue, 1, p_priority_queue.LAST);
            p_priority_queue.DELETE(p_priority_queue.LAST);
            sift_down(p_priority_queue, 1);
            RETURN v_tmp;
        EXCEPTION 
            WHEN NO_DATA_FOUND THEN DBMS_OUTPUT.PUT_LINE('PRIORITY QUEUE IS EMPTY!!!');
        END pop;
    
    FUNCTION top(p_priority_queue IN OUT priority_queue) RETURN priority_arr2d_type IS
        BEGIN
            RETURN p_priority_queue(1);
        EXCEPTION 
            WHEN NO_DATA_FOUND THEN DBMS_OUTPUT.PUT_LINE('PRIORITY QUEUE IS EMPTY!!!');
        END top;
    
    PROCEDURE print(p_priority_queue IN priority_queue) IS
        BEGIN
            IF p_priority_queue.COUNT = 0 THEN 
                RAISE NO_DATA_FOUND; 
            END IF;
            FOR i IN 1..p_priority_queue.COUNT LOOP
                DBMS_OUTPUT.PUT_LINE('priority: ' || p_priority_queue(i).priority);
                FOR j IN 1..p_priority_queue(i).arr.COUNT LOOP
                    FOR k IN 1..p_priority_queue(i).arr(j).COUNT LOOP
                        DBMS_OUTPUT.PUT(p_priority_queue(i).arr(j)(k) || ',');
                    END LOOP;
                    DBMS_OUTPUT.NEW_LINE;
                END LOOP;
                DBMS_OUTPUT.NEW_LINE;
            END LOOP;
        EXCEPTION 
            WHEN NO_DATA_FOUND THEN DBMS_OUTPUT.PUT_LINE('PRIORITY QUEUE IS EMPTY!!!');
        END print;
    
END priority_queue_package;


/


CREATE OR REPLACE PACKAGE priority_queue_package IS
    
    TYPE arr1d_type IS TABLE OF PLS_INTEGER INDEX BY PLS_INTEGER;
    TYPE arr2d_type IS TABLE OF arr1d_type INDEX BY PLS_INTEGER;
    TYPE priority_arr2d_type IS RECORD (arr arr2d_type, i_empty PLS_INTEGER, j_empty PLS_INTEGER, 
        depth PLS_INTEGER, priority PLS_INTEGER);
    TYPE priority_queue IS TABLE OF priority_arr2d_type INDEX BY PLS_INTEGER;
    
    PROCEDURE push(p_priority_queue IN OUT priority_queue, p_arr IN arr2d_type, p_i_empty IN PLS_INTEGER, 
        p_j_empty IN PLS_INTEGER, p_depth IN PLS_INTEGER, p_priority IN PLS_INTEGER);
    
    FUNCTION pop(p_priority_queue IN OUT priority_queue) RETURN priority_arr2d_type;
    
    FUNCTION top(p_priority_queue IN OUT priority_queue) RETURN priority_arr2d_type;
    
    PROCEDURE print(p_priority_queue IN priority_queue);
    
END priority_queue_package;


/


CREATE OR REPLACE SYNONYM pr_q FOR priority_queue_package;