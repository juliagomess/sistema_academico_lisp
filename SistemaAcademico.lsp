(defun add (lista prim)
    (if (eq (car lista) prim)
        (add (cdr lista) prim)

        (if (null lista)
            (cons prim (cons prim nil))
            (cons (car lista) (add (cdr lista) prim))
        )
    )
)

(defun retira-duplicado (lista)
    (if (null lista)
        nil
        (if (eq (car lista) (cadr lista))
            (retira-duplicado (cdr lista))
            (cons (car lista) (retira-duplicado (cdr lista)))
        )
    )
)

(defun adiciona (antigos novos)
    (if (null antigos)
        (if (null novos)
            nil
            (cons (car novos) (adiciona antigos (cdr novos)))
        )

        (if (eq (car antigos) (car novos))
            (adiciona antigos (retira-duplicado (cdr novos)))

            (if (eq (car novos) (cadr novos))
                (cons (car antigos) (adiciona (cdr antigos) 
                (retira-duplicado (cdr novos))))

                (adiciona antigos (add novos (car novos))) 
            )
        ) 
    )
)

(defun remove (antigos novos)
    (if (null antigos)
        nil

        (if (eq (car antigos) (car novos))
            (remove (cdr antigos) (retira-duplicado (cdr novos)))

            (if (eq (car novos) (cadr novos))  
                (cons (car antigos) (remove (cdr antigos) 
                (retira-duplicado (cdr novos))))
                
                (remove antigos (add novos (car novos)))
            )
        )
    ) 
)

(defun busca (lista item)
    (if (null lista)
        nil

        (if (eq (car lista) item)
            T
            (busca (cdr lista) item)
        )
    )
)

(defun matricular (alunos disciplinas bd) 
    (if (null bd)
        (if (null disciplinas)
            nil
    
            (cons 
                (cons (car disciplinas) (cons alunos 
                (cons nil (cons nil nil))))
                (matricular alunos (cdr disciplinas) bd)
            )
        )
        
        (if (eq (car disciplinas) (caar bd))
            (if (null (cadar bd))
                (cons 
                    (cons (car disciplinas) (cons alunos
                    (cons (caddar bd) (cons (car (cdddar bd)) nil))))
                    (matricular alunos (retira-duplicado (cdr disciplinas)) 
                    (cdr bd))
                )
                
                (cons 
                    (cons (car disciplinas) (cons (adiciona (cadar bd) 
                    alunos) (cons (caddar bd) (cons (car (cdddar bd)) nil))))
                    (matricular alunos (retira-duplicado (cdr disciplinas)) 
                    (cdr bd))
                )
            )
            
            (if (eq (car disciplinas) (cadr disciplinas))
                (cons (car bd) (matricular alunos (retira-duplicado 
                (cdr disciplinas)) (cdr bd)))

                (matricular alunos (add disciplinas 
                (car disciplinas)) bd)
            )
        )
    )
)

(defun cancelar-matricula (alunos disciplinas bd)
    (if (null bd)
        nil

        (if (eq (car disciplinas) (caar bd))
            (if (and (null (caddar bd)) (null (remove (cadar bd) alunos)))
                (cancelar-matricula alunos (retira-duplicado 
                (cdr disciplinas)) (cdr bd))

                (cons 
                    (cons (car disciplinas) (cons (remove (cadar bd) alunos) 
                    (cons (caddar bd) (cons (car (cdddar bd))
                    nil)))) (cancelar-matricula alunos (retira-duplicado 
                    (cdr disciplinas)) (cdr bd))
                ) 
            )

            (if (eq (car disciplinas) (cadr disciplinas))
                (cons (car bd) (cancelar-matricula alunos (retira-duplicado
                (cdr disciplinas)) (cdr bd)))

                (cancelar-matricula alunos (add disciplinas 
                (car disciplinas)) bd)
            )
        )
    )
)

(defun vincular (professores disciplinas bd) 
    (if (null bd)
        (if (null disciplinas)
            nil
    
            (cons 
                (cons (car disciplinas) (cons nil 
                (cons professores (cons nil nil))))
                (vincular professores (cdr disciplinas) bd)
            )
        )
        
        (if (eq (car disciplinas) (caar bd))
            (if (null (caddar bd))
                (cons 
                    (cons (car disciplinas) (cons (cadar bd) 
                    (cons professores (cons (car (cdddar bd)) nil))))
                    (vincular professores (retira-duplicado 
                    (cdr disciplinas)) (cdr bd))
                )
                
                (cons 
                    (cons (car disciplinas) (cons (cadar bd) 
                    (cons (adiciona (caddar bd) professores) 
                    (cons (car (cdddar bd)) nil))))
                    (vincular professores (retira-duplicado 
                    (cdr disciplinas)) (cdr bd))
                )
            )
            
            (if (eq (car disciplinas) (cadr disciplinas))
                (cons (car bd) (vincular professores (retira-duplicado 
                (cdr disciplinas)) (cdr bd)))

                (vincular professores (add disciplinas 
                (car disciplinas)) bd)
            )
        )
    )
)

(defun remover-vinculo (professores disciplinas bd)
    (if (null bd)
        nil

        (if (eq (car disciplinas) (caar bd))
            (if (and (null (cadar bd)) (null (remove (caddar bd) 
            professores)))
                (remover-vinculo professores (retira-duplicado 
                (cdr disciplinas)) (cdr bd))

                (cons 
                    (cons (car disciplinas) (cons (cadar bd) 
                    (cons (remove (caddar bd) professores) 
                    (cons (car (cdddar bd)) 
                    nil)))) (remover-vinculo professores 
                    (retira-duplicado (cdr disciplinas)) (cdr bd))
                ) 
            )
            

            (if (eq (car disciplinas) (cadr disciplinas))
                (cons (car bd) (remover-vinculo professores 
                (retira-duplicado (cdr disciplinas)) (cdr bd)))

                (remover-vinculo professores (add disciplinas 
                (car disciplinas)) bd)
            )
        )
    )
)

(defun vincular-disc-curso (disciplina curso bd) 
    (if (null bd)
        nil
        
        (if (eq disciplina (caar bd))
            (if (null (car (cdddar bd)))
                (cons 
                    (cons disciplina (cons (cadar bd) 
                    (cons (caddar bd) (cons curso nil))))
                    (vincular-disc-curso disciplina curso (cdr bd))
                )
                
                (cons (car bd) (vincular-disc-curso disciplina curso 
                (cdr bd)))
            )
            
            (cons (car bd) (vincular-disc-curso disciplina curso (cdr bd)))
        )
    )
)

(defun alunos? (bd)
    (if (null bd)
        nil

        (if (null (cadar bd))
            (alunos? (cdr bd))

            (adiciona (cadar bd) (alunos? (cdr bd)))
        )
    )
)

(defun professores? (bd)
    (if (null bd)
        nil

        (if (null (caddar bd))
            (professores? (cdr bd))

            (adiciona (caddar bd) (professores? (cdr bd)))
        )
    )
)

(defun disciplinas? (bd)
    (if (null bd)
        nil

        (if (null (caar bd))
            (disciplinas? (cdr bd))

            (cons (caar bd) (disciplinas? (cdr bd)))
        )
    )
)

(defun matriculados? (disciplina bd)
    (if (null bd)
        nil

        (if (eq disciplina (caar bd))
            (cadar bd)

            (matriculados? disciplina (cdr bd))
        )
    )
)

(defun vinculados? (disciplina bd)
    (if (null bd)
        nil

        (if (eq disciplina (caar bd))
            (caddar bd)

            (vinculados? disciplina (cdr bd))
        )
    )
)

(defun cursa? (aluno bd)
    (if (null bd)
        nil

        (if (busca (cadar bd) aluno)
            (cons (caar bd) (cursa? aluno (cdr bd)))

            (cursa? aluno (cdr bd))
        )
    )
)

(defun ministra? (professor bd)
    (if (null bd)
        nil

        (if (busca (caddar bd) professor)
            (cons (caar bd) (ministra? professor (cdr bd)))

            (ministra? professor (cdr bd))
        )
    )
)

(defun disc-curso? (disciplina bd) 
    (if (null bd)
        nil

        (if (eq disciplina (caar bd))
            (car (cdddar bd))

            (disc-curso? disciplina (cdr bd))
        )
    )
)