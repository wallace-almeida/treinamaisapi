package com.treinamaisapi.entity.enums.pacotes;

public enum StatusCompra {
    PENDENTE,     // pagamento iniciado (pix/boleto/cartão aguardando)
    APROVADA,     // pagamento confirmado → acesso liberado
    CANCELADA,    // cancelamento manual dentro do prazo (sem reembolso automático)
    REEMBOLSADA,  // dinheiro devolvido
    EXPIRADA      // plano acabou ou prazo vencido
}
