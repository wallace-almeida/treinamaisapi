

package com.treinamaisapi.service.cartao;

import com.treinamaisapi.common.dto.flashcard.cartao.CartaoRequest;
import com.treinamaisapi.common.dto.flashcard.cartao.CartaoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.FlashcardEstudoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.RevisaoPendenteResponse;
import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.cartao.Cartao;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.BaralhoRepository;
import com.treinamaisapi.repository.CapituloRepository;
import com.treinamaisapi.repository.CartaoRepository;
import com.treinamaisapi.repository.TemaRepository;
import com.treinamaisapi.service.gamificacao.interfac.GamificacaoService;
import com.treinamaisapi.service.gamificacao.service.GamificacaoServiceImpl;

import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;


@Service
@RequiredArgsConstructor
public class CartaoService {

    private final CartaoRepository cartaoRepository;
    private final TemaRepository temaRepository;
    private final BaralhoRepository baralhoRepository;
    private  final GamificacaoService gamificacaoService;

    public CartaoResponse criarManual(Long usuarioId, CartaoRequest req) {
        Tema tema = temaRepository.findById(req.temaId())
                .orElseThrow(() -> new NotFoundException("Tema não encontrado"));

        Baralho baralho = null;

        if (req.baralhoId() != null) {
            baralho = baralhoRepository.findById(req.baralhoId())
                    .orElseThrow(() -> new NotFoundException("Baralho não encontrado"));
        }


        Cartao cartao = Cartao.builder()
                .frente(req.frente())
                .verso(req.verso())
                .tema(tema)
                .usuario(new Usuario(usuarioId))
                .baralho(baralho)
                .build();

        cartaoRepository.save(cartao);

        return toResponse(cartao);
    }



    @Transactional
    public FlashcardEstudoResponse revisar(Long usuarioId, Long cartaoId, int qualidade) {

        Cartao c = cartaoRepository.findByUsuarioIdAndId(usuarioId, cartaoId)
                .orElseThrow(() -> new NotFoundException("Cartão não encontrado"));

        if (c.getBaralho() == null) {
            throw new BusinessException("Este cartão não está associado a um baralho");
        }

        LocalDateTime agora = LocalDateTime.now();

        int repeticoes = Optional.ofNullable(c.getRepeticoes()).orElse(0);
        double ef = Optional.ofNullable(c.getFatorFacilidade()).orElse(2.5);

        // Algoritmo SM-2 (Ajustado) + Regra de 20 min para < 3
        if (qualidade < 3) {
            repeticoes = 0;
            c.setIntervaloDias(0);

            // ✅ volta depois (evita repetir imediatamente)
            c.setProximaRevisao(agora.plusMinutes(20));

        } else {
            if (repeticoes == 0) {
                c.setIntervaloDias(1);
            } else if (repeticoes == 1) {
                c.setIntervaloDias(6);
            } else {
                int novoIntervalo = (int) Math.round(c.getIntervaloDias() * ef);
                c.setIntervaloDias(Math.min(novoIntervalo, 36500));
            }

            repeticoes++;
            ef = Math.max(
                    1.3,
                    ef + (0.1 - (5 - qualidade) * (0.08 + (5 - qualidade) * 0.02))
            );

            // ✅ revisão normal (em dias)
            c.setProximaRevisao(agora.plusDays(c.getIntervaloDias()));
        }

        c.setRepeticoes(repeticoes);
        c.setFatorFacilidade(ef);
        c.setUltimaRevisao(agora);

        cartaoRepository.save(c);

        gamificacaoService.processarEstudoFlashcard(
                c.getUsuario(),
                c.getBaralho().getId(),
                1,
                1
        );

        // Contagens atualizadas
        int pendentesHoje = Math.toIntExact(cartaoRepository.contarPendentesHoje(usuarioId));

        LocalDateTime inicio = java.time.LocalDate.now().atStartOfDay();
        LocalDateTime fim = inicio.plusDays(1);

        int revisadosHoje = Math.toIntExact(
                cartaoRepository.contarRevisadosHoje(usuarioId, inicio, fim)
        );

        // Buscar próximo cartão (PageRequest para 1)
        List<Cartao> result = cartaoRepository.buscarProximoParaEstudo(
                usuarioId,
                LocalDateTime.now(),
                PageRequest.of(0, 1)
        );

        Optional<Cartao> next = result.stream().findFirst();

        return next.map(n -> new FlashcardEstudoResponse(
                n.getId(),
                n.getFrente(),
                n.getQuestao().getExplicacao(),
                pendentesHoje,
                revisadosHoje,
                calcularMetaPercentual(pendentesHoje, revisadosHoje)
        )).orElseGet(() -> new FlashcardEstudoResponse(
                null,
                null,
                null,
                pendentesHoje,
                revisadosHoje,
                calcularMetaPercentual(pendentesHoje, revisadosHoje)
        ));
    }


    @Transactional(readOnly = true)
    public FlashcardEstudoResponse buscarProximoParaEstudo(Long userId) {

        int pendentesHoje = Math.toIntExact(cartaoRepository.contarPendentesHoje(userId));

        LocalDateTime inicio = LocalDate.now().atStartOfDay();
        LocalDateTime fim = inicio.plusDays(1);

        int revisadosHoje = Math.toIntExact(
                cartaoRepository.contarRevisadosHoje(userId, inicio, fim)
        );

        int meta = calcularMetaPercentual(pendentesHoje, revisadosHoje);

        List<Cartao> result = cartaoRepository.buscarProximoParaEstudo(
                userId,
                LocalDateTime.now(),
                PageRequest.of(0, 1)
        );

        if (result.isEmpty()) {
            return new FlashcardEstudoResponse(
                    null,
                    null,
                    null,
                    pendentesHoje,
                    revisadosHoje,
                    meta
            );
        }

        Cartao c = result.get(0);

        String verso = (c.getQuestao() != null) ? c.getQuestao().getExplicacao() : null;

        return new FlashcardEstudoResponse(
                c.getId(),
                c.getFrente(),
                verso,
                pendentesHoje,
                revisadosHoje,
                meta
        );
    }




    private int calcularMetaPercentual(int pendentes, int revisados) {
        int total = pendentes + revisados;
        if (total == 0) return 100;
        return (revisados * 100) / total;
    }






    private CartaoResponse toResponse(Cartao c) {
        return new CartaoResponse(c.getId(), c.getFrente(), c.getVerso(), c.isPrecisaRevisar());
    }
}

