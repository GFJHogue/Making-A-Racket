;; A DAG is a (listof (list Sym (listof Sym)))

;; An NTree is a (list Sym (listof NTree))

(define graph0 '((A (C))
                 (B (D F))
                 (C (F G))
                 (D ())
                 (E (D G))
                 (F (H))
                 (G ())
                 (H ())))

(define graph1 '((A (B))
                 (B (C D E))
                 (C (E))
                 (D (E))
                 (E (F))
                 (F ())
                 (G (D))))

(define graph2 '((A (B))
                 (B (D E))
                 (C (A D))
                 (D (E))
                 (E ())
                 (F (A C))))

(define graph3 '((A (B C))
                 (B (D E))
                 (C (F))
                 (D (F))
                 (E (C F))
                 (F ())))


;; (remove-node graph node) produces graph with node removed
;; remove-node : DAG Sym -> DAG

(define (remove-node graph node)
  (local [(define not-node
            (lambda (n)
              (not (symbol=? n node))))]
    
    (foldr (lambda (ns lstns)
             (cons (list (first ns)
                         (filter not-node
                                 (second ns)))
                   lstns))
           empty
           (filter (lambda (ns)
                     (not-node (first ns)))
                   graph))))


;; (add-node graph node in out) produces graph with new node with in and out
;;     neighbours
;; add-node : DAG Sym (listof Sym) (listof Sym) -> DAG

(define (add-node graph node in out)
  (foldr (lambda (ns lstns)
           (cons (list (first ns)
                       (cond [(member? (first ns) in)
                              (cons node (second ns))]
                             [else (second ns)]))
                 lstns))
         (list (list node out))
         graph))


;; (edit-node graph node in out) produces graph with node with in an out
;;     neighbours
;; edit-node : DAG Sym (listof Sym) (listof Sym) -> DAG

(define (edit-node graph node in out)
  (add-node (remove-node graph node)
            node in out))


;; (o-neighbours graph node) produces the list of out neighbours of node in graph
;; o-neighbours : DAG Sym -> (listof Sym)

(define (o-neighbours graph node)
  (foldr (lambda (ns lstn)
           (cond [(symbol=? (first ns) node)
                  (second ns)]
                 [else lstn]))
         empty
         graph))


;; (i-neighbours graph node) produces the list of in neighbours of node in graph
;; i-neighbours : DAG Sym -> (listof Sym)

(define (i-neighbours graph node)
  (foldr (lambda (ns lstn)
           (cond [(member? node (second ns))
                  (cons (first ns) lstn)]
                 [else lstn]))
         empty
         graph))


;; (get-nodes graph) produces the list of all nodes in graph
;; get-nodes : DAG -> (listof Sym)

(define (get-nodes graph)
  (foldr (lambda (ns lstn)
           (cons (first ns) lstn))
         empty
         graph))


;; (tree-at-node graph node neighbours) produces the tree of all neighbour paths
;;     of node in graph
;; tree-at-node : DAG Sym (DAG Sym -> (listof Sym)) -> NTree

(define (tree-at-node graph node neighbours)
  (filter (lambda (x)
            (not (empty? x)))
          (list node
                (foldr (lambda (n lstns)
                         (cons (tree-at-node graph n neighbours)
                               lstns))
                       empty
                       (neighbours graph node)))))


;; (tree->paths tree) produces all paths from tree
;; tree->paths : NTree -> (listof (listof Sym))

(define (tree->paths tree)
  (cond [(empty? (rest tree))
         (list tree)]
        [else
         (foldr (lambda (t lstp)
                  (append (foldr (lambda (p lstp)
                                   (cons (cons (first tree) p)
                                         lstp))
                                 empty
                                 (tree->paths t))
                          lstp))
                empty
                (second tree))]))


;; (shortest-path graph node1 node2 neighbours) produces the shortest path
;;     between neighbours node1 and node 2
;; shortest-path : DAG Sym Sym (DAG Sym -> (listof Sym)) -> (listof Sym)
#|
(define (shortest-path graph node1 node2 neighbours)
  ((filter (lambda (p)
            (member? node2 p))
          (tree->paths (tree-at-node graph node1 neighbours)))))|#
  
