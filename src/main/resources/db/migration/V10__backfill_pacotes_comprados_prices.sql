UPDATE pacotes_comprados pc
SET
  preco_original = COALESCE(pc.preco_original, p.preco),
  valor_desconto = COALESCE(pc.valor_desconto, 0),
  preco_final = COALESCE(pc.preco_final, COALESCE(pc.preco_original, p.preco))
FROM pacotes p
WHERE pc.pacote_id = p.id
  AND (pc.preco_original IS NULL OR pc.valor_desconto IS NULL OR pc.preco_final IS NULL);
