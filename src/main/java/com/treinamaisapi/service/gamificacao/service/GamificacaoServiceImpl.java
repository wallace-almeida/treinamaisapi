package com.treinamaisapi.service.gamificacao.service;

import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.HistoricoEstudoRepository;
import com.treinamaisapi.repository.PontuacaoRepository;
import com.treinamaisapi.repository.QuestaoSimuladoRepository;
import com.treinamaisapi.service.gamificacao.interfac.GamificacaoService;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.time.Duration;

@Service
@Transactional
public class GamificacaoServiceImpl implements GamificacaoService {

    // ========= CONSTANTES =========
    private static final int XP_BASE_SIMULADO = 10;
    private static final int XP_POR_QUESTAO = 2;
    private static final int XP_POR_ACERTO = 5;

    private static final int XP_FLASHCARD_BASE = 10;
    private static final int XP_POR_CARTAO = 1;

    private static final int XP_POR_NIVEL = 500;

    private final PontuacaoRepository pontuacaoRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;
    private final QuestaoSimuladoRepository questaoSimuladoRepository;

    public GamificacaoServiceImpl(
            PontuacaoRepository pontuacaoRepository,
            HistoricoEstudoRepository historicoEstudoRepository,
            QuestaoSimuladoRepository questaoSimuladoRepository
    ) {
        this.pontuacaoRepository = pontuacaoRepository;
        this.historicoEstudoRepository = historicoEstudoRepository;
        this.questaoSimuladoRepository = questaoSimuladoRepository;
    }

    // ================= SIMULADO =================

    @Override
    public void processarConclusaoSimulado(Simulado simulado) {

        // ✅ DADOS REAIS DO BANCO
        int totalQuestoes =
                questaoSimuladoRepository.countBySimuladoId(simulado.getId());

        int totalAcertos =
                questaoSimuladoRepository.countBySimuladoIdAndCorretaTrue(simulado.getId());

        // ⏱️ TEMPO REAL
        long tempoMinutos = Duration.between(
                simulado.getDataCriacao(),
                simulado.getDataFinalizacao()
        ).toMinutes();

        if (tempoMinutos < 0) tempoMinutos = 0;

        double multiplicador = calcularMultiplicadorTempo(tempoMinutos);

        int xpFinal = (int) Math.round(
                (
                        XP_BASE_SIMULADO +
                                (totalQuestoes * XP_POR_QUESTAO) +
                                (totalAcertos * XP_POR_ACERTO)
                ) * multiplicador
        );

        atualizarPontuacao(simulado.getUsuario(), xpFinal);

        registrarHistorico(
                simulado.getUsuario(),
                TipoAtividade.SIMULADO,
                simulado.getId(),
                tempoMinutos
        );
    }

    // ================= FLASHCARD =================

    @Override
    public void processarEstudoFlashcard(
            Usuario usuario,
            Long baralhoId,
            int quantidadeCartoes,
            int tempoMinutos
    ) {

        int xpFinal =
                XP_FLASHCARD_BASE +
                        (quantidadeCartoes * XP_POR_CARTAO);

        atualizarPontuacao(usuario, xpFinal);

        registrarHistorico(
                usuario,
                TipoAtividade.BARALHO,
                baralhoId,
                tempoMinutos
        );
    }

    // ================= AUXILIARES =================

    private void atualizarPontuacao(Usuario usuario, int xpGanho) {

        Pontuacao pontuacao = pontuacaoRepository
                .findByUsuario(usuario)
                .orElseGet(() -> Pontuacao.builder()
                        .usuario(usuario)
                        .total(0.0)
                        .nivelAtual(1)
                        .build()
                );

        pontuacao.setTotal(pontuacao.getTotal() + xpGanho);
        pontuacao.setNivelAtual(calcularNivel(pontuacao.getTotal()));

        pontuacaoRepository.save(pontuacao);
    }

    private int calcularNivel(double xpTotal) {
        return (int) (xpTotal / XP_POR_NIVEL) + 1;
    }

    private double calcularMultiplicadorTempo(long minutos) {
        if (minutos < 5) return 0.8;
        if (minutos <= 15) return 1.0;
        if (minutos <= 40) return 1.2;
        return 1.4;
    }

    private void registrarHistorico(
            Usuario usuario,
            TipoAtividade tipo,
            Long referenciaId,
            long tempoMin
    ) {
        HistoricoEstudo historico = HistoricoEstudo.builder()
                .usuario(usuario)
                .tipoAtividade(tipo)
                .referenciaId(referenciaId)
                .tempoEstudoMinutos(tempoMin) // Long ✔
                .build();

        historicoEstudoRepository.save(historico);
    }
}
