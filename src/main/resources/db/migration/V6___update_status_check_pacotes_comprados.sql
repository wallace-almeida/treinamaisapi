-- Remove o CHECK antigo
ALTER TABLE pacotes_comprados
DROP CONSTRAINT IF EXISTS pacotes_comprados_status_check;

-- Recria o CHECK incluindo REEMBOLSO_SOLICITADO
ALTER TABLE pacotes_comprados
ADD CONSTRAINT pacotes_comprados_status_check
CHECK (
  (status)::text = ANY (
    (ARRAY[
      'CRIADA'::character varying,
      'PENDENTE'::character varying,
      'APROVADA'::character varying,
      'CANCELADA'::character varying,
      'EXPIRADA'::character varying,
      'REEMBOLSADA'::character varying,
      'REEMBOLSO_SOLICITADO'::character varying
    ])::text[]
  )
);
