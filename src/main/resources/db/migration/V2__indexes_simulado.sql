/* =========================================================
   Índices para fluxo de filtros e montagem de simulado
   ========================================================= */

/* ---------------------------------------------------------
   pacote_temas
   - PK já é (pacote_id, tema_id)
   - este índice acelera joins partindo de tema
   --------------------------------------------------------- */
CREATE INDEX IF NOT EXISTS idx_pacote_temas_tema_id
ON public.pacote_temas (tema_id);


/* ---------------------------------------------------------
   pacotes_comprados
   - primeira query do fluxo
   - filtra por usuario + ativo + status
   --------------------------------------------------------- */
CREATE INDEX IF NOT EXISTS idx_pacotes_comprados_usuario_ativo_status
ON public.pacotes_comprados (usuario_id, ativo, status);


/* ---------------------------------------------------------
   Árvore Tema -> Capítulo -> Subcapítulo
   --------------------------------------------------------- */
CREATE INDEX IF NOT EXISTS idx_capitulo_tema_id
ON public.capitulo (tema_id);

CREATE INDEX IF NOT EXISTS idx_subcapitulo_capitulo_id
ON public.sub_capitulo (capitulo_id);


/* ---------------------------------------------------------
   Questões
   - índice simples por subcapitulo_id JÁ EXISTE
   - abaixo são índices compostos (upgrade real)
   --------------------------------------------------------- */
CREATE INDEX IF NOT EXISTS idx_questoes_subcapitulo_banca
ON public.questoes (subcapitulo_id, banca);

CREATE INDEX IF NOT EXISTS idx_questoes_subcapitulo_nivel
ON public.questoes (subcapitulo_id, nivel_dificuldade);


/* ---------------------------------------------------------
   ElementCollections do Simulado
   - melhora leitura futura de simulados já criados
   --------------------------------------------------------- */
CREATE INDEX IF NOT EXISTS idx_simulado_tema_simulado_id
ON public.simulado_tema_ids (simulado_id);

CREATE INDEX IF NOT EXISTS idx_simulado_capitulo_simulado_id
ON public.simulado_capitulo_ids (simulado_id);

CREATE INDEX IF NOT EXISTS idx_simulado_subcapitulo_simulado_id
ON public.simulado_subcapitulo_ids (simulado_id);

CREATE INDEX IF NOT EXISTS idx_simulado_bancas_simulado_id
ON public.simulado_bancas (simulado_id);

CREATE INDEX IF NOT EXISTS idx_simulado_niveis_simulado_id
ON public.simulado_niveis (simulado_id);


/* =========================================================
   Execução e resposta de simulados
   ========================================================= */

-- questoes_respondidas
-- (mantém tudo igual acima)

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint c
    JOIN pg_class t ON t.oid = c.conrelid
    JOIN pg_namespace n ON n.oid = t.relnamespace
    WHERE c.conname = 'uk_qr_simulado_questao'
      AND n.nspname = 'public'
      AND t.relname = 'questoes_respondidas'
  ) THEN
    ALTER TABLE public.questoes_respondidas
      ADD CONSTRAINT uk_qr_simulado_questao
      UNIQUE (simulado_id, questao_id);
  END IF;
END$$;

-- opcional, mas muito útil
CREATE INDEX IF NOT EXISTS idx_simulados_usuario_status_data
ON public.simulados (usuario_id, status, data_criacao DESC);


/* =========================================================
   Histórico de questões do usuário
   ========================================================= */

CREATE INDEX IF NOT EXISTS idx_qhu_usuario_data
ON public.questao_historico_usuario (usuario_id, data_resposta DESC);

CREATE INDEX IF NOT EXISTS idx_qhu_usuario_questao
ON public.questao_historico_usuario (usuario_id, questao_id);

-- opcional (se filtrar por tema)
-- CREATE INDEX IF NOT EXISTS idx_qhu_usuario_tema
-- ON public.questao_historico_usuario (usuario_id, tema_id);
