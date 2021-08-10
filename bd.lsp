(setq bd nil)

(setq bd (matricular '(alunos) '(disciplinas) bd))
(setq bd (cancelar-matricula '(alunos) '(disciplinas) bd))
(setq bd (vincular '(professores) '(disciplinas) bd))
(setq bd (remover-vinculo '(professores) '(disciplinas) bd))
(setq bd (vincular-disc-curso 'disciplina 'curso bd))

(alunos? bd)
(professores? bd)
(disciplinas? bd)

(matriculados? 'disciplina bd)
(vinculados? 'disciplina bd)
(cursa? 'aluno bd)
(ministra? 'professor bd)
(disc-curso? 'disciplina bd)