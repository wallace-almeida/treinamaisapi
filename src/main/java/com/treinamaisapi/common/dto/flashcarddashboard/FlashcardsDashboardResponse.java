package com.treinamaisapi.common.dto.flashcarddashboard;

import java.util.List;

public record FlashcardsDashboardResponse(
        int pendentesHoje,
        int metaDiariaPercentual,
        List<FlashcardBaralhoResumo> baralhos
) {}
