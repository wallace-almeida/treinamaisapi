/* =========================================================
   Índice para otimizar busca de questões mais erradas
   =========================================================
   Usado em:
   - QuestaoHistoricoUsuarioRepository.findQuestoesMaisErradas
   - filtros por usuário + acertou=false + questao_id
   ========================================================= */

CREATE INDEX IF NOT EXISTS idx_qhu_usuario_acertou_questao
ON public.questao_historico_usuario (usuario_id, acertou, questao_id);
