package com.treinamaisapi.service.simulado;


import com.treinamaisapi.common.dto.compra.response.PacoteCompradoComUsuarioDTO;
import com.treinamaisapi.common.dto.simulado.filtro.CapituloFiltroDTO;
import com.treinamaisapi.common.dto.simulado.filtro.PacoteFiltroSimuladoDTO;
import com.treinamaisapi.common.dto.simulado.filtro.SubcapituloFiltroDTO;
import com.treinamaisapi.common.dto.simulado.filtro.TemaFiltroDTO;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaQuestaoSimulado;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.*;
import com.treinamaisapi.common.exception.BusinessException;
import com.treinamaisapi.common.exception.NotFoundException;
import com.treinamaisapi.common.filtroAuxil.BancaPorPacoteProjection;
import com.treinamaisapi.common.filtroAuxil.FiltroArvoreLinhaProjection;
import com.treinamaisapi.common.filtroAuxil.NivelPorPacoteProjection;
import com.treinamaisapi.common.filtroAuxil.PacoteHeaderProjection;
import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.cartao.Cartao;
import com.treinamaisapi.entity.enums.StatusSimulado;
import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.historico_estudo.HistoricoEstudo;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.questao_historico_usuario.QuestaoHistoricoUsuario;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;

import com.treinamaisapi.repository.*;
import com.treinamaisapi.service.compra.pacote.PacoteCompradoService;
import com.treinamaisapi.service.gamificacao.interfac.GamificacaoService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoBalanceService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoFraquezaService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoHistoricoService;
import com.treinamaisapi.service.simulado.auxiliar.QuestaoSelectorService;
import com.treinamaisapi.spec.QuestaoSpecification;
import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SimuladoService {

    private final SimuladoRepository simuladoRepository;
    private final QuestaoRepository questaoRepository;
    private final QuestaoSimuladoRepository questaoSimuladoRepository;
    private final UsuarioRepository usuarioRepository;
    private final HistoricoEstudoRepository historicoEstudoRepository;
    private final PacoteCompradoService pacoteCompradoService;
    private final PacoteCompradoRepository pacoteCompradoRepository;
    private final QuestaoSelectorService questaoSelectorService;
    private final QuestaoBalanceService questaoBalanceService;
    private final QuestaoHistoricoService questaoHistoricoService;
    private final QuestaoFraquezaService questaoFraquezaService;
    private final QuestaoHistoricoUsuarioRepository questaoHistoricoUsuarioRepository;
    private final GamificacaoService gamificacaoService;
    private final CartaoRepository cartaoRepository;
    private final BaralhoRepository baralhoRepository;
    private  final PacoteFiltroRepository pacoteFiltroRepository;


    @Transactional
    public SimuladoExecucaoResponse criarSimulado(CriarSimuladoRequest request, Long usuarioId) {

        // 1) Carrega usuário
        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new NotFoundException("Usuário não encontrado."));

        // 2) Valida acesso (pacote ativo para o concurso)
        PacoteCompradoComUsuarioDTO pacoteDTO = pacoteCompradoService.listarComprasAtivas(usuarioId).stream()
                .filter(c -> c.getConcursoId().equals(request.getConcursoId()))
                .findFirst()
                .orElseThrow(() -> new BusinessException("Usuário não possui acesso a este concurso."));

        // 3) Define título automático do simulado baseado no pacote
        String tituloSimulado = "Simulado " + pacoteDTO.getNomePacote();

        // 4) Define quantidade de questões (default = 10)
        int quantidadeTotal = request.getQuantidadeQuestoes() == null
                ? 10
                : request.getQuantidadeQuestoes();

        // 5) Busca questões baseadas nos filtros (tema/capítulo/subcapítulo/banca/nível...)
        List<Questao> questoesFiltradas = questaoRepository.findAll(
                QuestaoSpecification.filtrar(request)
        );

        if (questoesFiltradas.isEmpty()) {
            throw new BusinessException(
                    "Não encontramos questões com os filtros selecionados. Tente ajustar os critérios."
            );
        }

        // Remove duplicadas por segurança
        questoesFiltradas = questoesFiltradas.stream()
                .distinct()
                .toList();

        // ----------------------------------------------------------------------
        // 6) Seleção base aleatória
        // ----------------------------------------------------------------------
        List<Questao> baseAleatoria = questaoSelectorService.selecionar(
                questoesFiltradas, usuario, quantidadeTotal, request
        );

        // ----------------------------------------------------------------------
        // 7) Fraquezas (questões mais erradas do usuário, dentro do mesmo pool)
        // ----------------------------------------------------------------------
        List<Questao> fraquezas = questaoFraquezaService.buscarQuestoesDeFraqueza(
                usuario, questoesFiltradas, quantidadeTotal
        );

        // ----------------------------------------------------------------------
        // 8) Combina fraquezas + base aleatória, sem duplicar
        //    (fraquezas têm prioridade, depois completamos com aleatórias)
        // ----------------------------------------------------------------------
        List<Questao> combinadas = new ArrayList<>();
        Set<Long> idsUsados = new HashSet<>();

        for (Questao q : fraquezas) {
            if (q.getId() != null && idsUsados.add(q.getId())) {
                combinadas.add(q);
            }
        }

        for (Questao q : baseAleatoria) {
            if (combinadas.size() >= quantidadeTotal) break;
            if (q.getId() != null && idsUsados.add(q.getId())) {
                combinadas.add(q);
            }
        }

        // Se por algum motivo ainda estiver vazia, usa o pool inteiro
        if (combinadas.isEmpty()) {
            combinadas = new ArrayList<>(questoesFiltradas);
            idsUsados.clear();
            idsUsados.addAll(
                    combinadas.stream()
                            .map(Questao::getId)
                            .filter(Objects::nonNull)
                            .toList()
            );
        }

        // ----------------------------------------------------------------------
        // 9) Histórico: evita repetir as ÚLTIMAS N questões
        //     (QuestaoHistoricoService já está configurado com PageRequest.of(0, N))
        // ----------------------------------------------------------------------
        combinadas = questaoHistoricoService.filtrarNaoRespondidas(usuario, combinadas);

        // Se o histórico eliminar tudo (poucas questões no banco, muito uso recente),
        // relaxamos a regra e voltamos para o pool completo.
        if (combinadas.isEmpty()) {
            combinadas = new ArrayList<>(questoesFiltradas);
        }

        // ----------------------------------------------------------------------
        // 10) Balanceamento de dificuldade (30% fácil, 50% médio, 20% difícil, etc.)
        // ----------------------------------------------------------------------
        List<Questao> questoesSelecionadas = questaoBalanceService.balancear(combinadas, request);

        // Remove duplicadas e limita à quantidade desejada
        questoesSelecionadas = questoesSelecionadas.stream()
                .distinct()
                .limit(quantidadeTotal)
                .toList();

        // Fallback: se ainda tiver menos que o desejado, completa com restantes do pool
        if (questoesSelecionadas.size() < quantidadeTotal) {
            Set<Long> idsSelecionadas = questoesSelecionadas.stream()
                    .map(Questao::getId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());

            List<Questao> restantes = questoesFiltradas.stream()
                    .filter(q -> q.getId() != null && !idsSelecionadas.contains(q.getId()))
                    .toList();

            List<Questao> mutaveis = new ArrayList<>(questoesSelecionadas);
            for (Questao q : restantes) {
                if (mutaveis.size() >= quantidadeTotal) break;
                mutaveis.add(q);
            }
            questoesSelecionadas = mutaveis;
        }

        if (questoesSelecionadas.isEmpty()) {
            throw new BusinessException(
                    "Não foi possível montar o simulado com os critérios selecionados."
            );
        }

        // ----------------------------------------------------------------------
        // 11) Cria o Simulado
        // ----------------------------------------------------------------------
        Simulado simulado = Simulado.builder()
                .titulo(tituloSimulado)
                .usuario(usuario)
                .quantidadeQuestoes(questoesSelecionadas.size())
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now())
                .status(StatusSimulado.EM_ANDAMENTO)
                .bancas(request.getBancas() != null ? new ArrayList<>(request.getBancas()) : null)
                .niveis(request.getNiveis() != null ? new ArrayList<>(request.getNiveis()) : null)
                .temaIds(request.getTemaIds() != null ? new ArrayList<>(request.getTemaIds()) : null)
                .capituloIds(request.getCapituloIds() != null ? new ArrayList<>(request.getCapituloIds()) : null)
                .subcapituloIds(request.getSubcapituloIds() != null ? new ArrayList<>(request.getSubcapituloIds()) : null)
                .inteligente(true)
                .balanceado(true)
                .prioridadeFraquezas(true)
                .build();

        simuladoRepository.save(simulado);

        // ----------------------------------------------------------------------
        // 12) Vincula questões ao simulado (QUESTOES_RESPONDIDAS / QuestaoSimulado)
        // ----------------------------------------------------------------------
        AtomicInteger ordem = new AtomicInteger(1);
        List<QuestaoSimulado> vinculadas = questoesSelecionadas.stream()
                .map(q -> QuestaoSimulado.builder()
                        .simulado(simulado)
                        .questao(q)
                        .pontuacaoObtida(0.0)
                        .respondida(false)
                        .ordem(ordem.getAndIncrement())
                        .build()
                ).toList();

        questaoSimuladoRepository.saveAll(vinculadas);

        // 13) Retorna simulado pronto para execução
        return SimuladoExecucaoResponse.fromEntity(simulado, vinculadas);
    }











    @Transactional(readOnly = true)
    public List<SimuladoResumoResponse> listarResumoSimulados(Long usuarioId) {
        LocalDateTime trintaDiasAtras = LocalDateTime.now().minusDays(15);

        return simuladoRepository
                .findByUsuarioIdAndStatusAndDataCriacaoAfterOrderByDataCriacaoDesc(usuarioId,StatusSimulado.FINALIZADO, trintaDiasAtras)
                .stream()
                .map(SimuladoResumoResponse::fromEntity)
                .toList();
    }

    @Transactional
    public ResultadoSimuladoResponse responderSimulado(Long simuladoId, RespostaSimuladoRequest request) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new NotFoundException("Simulado não encontrado"));

        if (simulado.getStatus() != StatusSimulado.EM_ANDAMENTO) {
            throw new BusinessException("Simulado já finalizado");
        }

        // Atualiza respostas do usuário
        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            QuestaoSimulado qs = questaoSimuladoRepository
                    .findBySimuladoIdAndQuestaoId(simuladoId, r.getQuestaoId())
                    .orElseThrow(() -> new NotFoundException("Questão não encontrada"));

            boolean correta = qs.getQuestao().getRespostaCorreta().equalsIgnoreCase(r.getRespostaUsuario());

            qs.setRespostaUsuario(r.getRespostaUsuario());
            qs.setCorreta(correta);
            qs.setRespondida(true);
            qs.setPontuacaoObtida(correta ? 1.0 : 0.0);

            questaoSimuladoRepository.save(qs);
        }

        // Finaliza simulado normalmente
        return finalizarSimulado(simulado, false);
    }

    @Transactional
    public Object buscarSimuladoAtivo(Long usuarioId) {

        Simulado simulado = simuladoRepository
                .findFirstByUsuarioIdAndStatus(usuarioId, StatusSimulado.EM_ANDAMENTO)
                .orElse(null);

        if (simulado == null) return null;

        LocalDateTime agora = LocalDateTime.now();
        LocalDateTime fimSimulado = simulado.getDataCriacao().plusMinutes(simulado.getTempoDuracao());

        if (agora.isAfter(fimSimulado)) {
            // Finaliza simulado e retorna resultado final
            ResultadoSimuladoResponse resultado = finalizarSimulado(simulado, true);
            return resultado; // <-- retorna o DTO finalizado
        }

        // Simulado ainda em andamento: retorna simulado completo
        List<QuestaoSimulado> questoesAtivas = questaoSimuladoRepository.findBySimuladoId(simulado.getId());
        return SimuladoExecucaoResponse.fromEntity(simulado, questoesAtivas);
    }



    @Transactional
    public ResultadoSimuladoResponse finalizarSimulado(Simulado simulado, boolean porTempo) {
        if (simulado.getStatus() == StatusSimulado.FINALIZADO) {
            return visualizarResultado(simulado.getId());
        }

        Long usuarioId = simulado.getUsuario().getId();
        LocalDateTime agora = LocalDateTime.now();
        LocalDateTime fimSimulado = porTempo && simulado.getTempoDuracao() != null
                ? simulado.getDataCriacao().plusMinutes(simulado.getTempoDuracao())
                : agora;

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simulado.getId());
        List<QuestaoHistoricoUsuario> historicos = new ArrayList<>();
        int acertos = 0;

        for (QuestaoSimulado qs : questoes) {
            if (!qs.getRespondida()) {
                qs.setRespondida(true);
                qs.setCorreta(false);
                qs.setRespostaUsuario(null);
                qs.setPontuacaoObtida(0.0);
            }

            boolean correta = Boolean.TRUE.equals(qs.getCorreta());
            if (correta) acertos++;

            Tema tema = qs.getQuestao().getSubcapitulo().getCapitulo().getTema();

            historicos.add(
                    QuestaoHistoricoUsuario.builder()
                            .usuario(simulado.getUsuario())
                            .questao(qs.getQuestao())
                            .dataResposta(fimSimulado)
                            .acertou(correta)
                            .simuladoId(simulado.getId())
                            .nivelDificuldade(qs.getQuestao().getNivelDificuldade())
                            .temaId(tema.getId())
                            .temaNome(tema.getNome())
                            .build()
            );

            // Cartão apenas para erros
            if (!correta && !cartaoRepository.existsByUsuarioIdAndQuestaoId(usuarioId, qs.getQuestao().getId())) {
                Baralho baralho = baralhoRepository.findByUsuarioIdAndTemaId(usuarioId, tema.getId())
                        .orElseGet(() -> baralhoRepository.save(
                                Baralho.builder()
                                        .titulo("Erros em " + tema.getNome())
                                        .tema(tema)
                                        .usuario(simulado.getUsuario())
                                        .build()
                        ));

                Cartao cartao = Cartao.builder()
                        .frente(qs.getQuestao().getEnunciado())
                        .verso(qs.getQuestao().getExplicacao())
                        .tema(tema)
                        .usuario(simulado.getUsuario())
                        .questao(qs.getQuestao())
                        .baralho(baralho)
                        .precisaRevisar(true)
                        .build();

                cartaoRepository.save(cartao);
            }
        }

        questaoSimuladoRepository.saveAll(questoes);
        questaoHistoricoUsuarioRepository.saveAll(historicos);

        double pontuacaoFinal = (acertos * 100.0) / questoes.size();

        // Calcula tempo de estudo
        long tempoEstudoMinutos;
        if (porTempo && simulado.getTempoDuracao() != null) {
            // Finalização automática: usa tempo planejado
            tempoEstudoMinutos = simulado.getTempoDuracao();
        } else {
            tempoEstudoMinutos = Math.max(0, Duration.between(simulado.getDataCriacao(), fimSimulado).toMinutes());
            if (simulado.getTempoDuracao() != null) {
                tempoEstudoMinutos = Math.min(tempoEstudoMinutos, simulado.getTempoDuracao());
            }
        }

        // Atualiza simulado
        simulado.setStatus(StatusSimulado.FINALIZADO);
        simulado.setPontuacaoFinal(pontuacaoFinal);
        simulado.setDataFinalizacao(fimSimulado);
        simuladoRepository.save(simulado);

        // Gamificação
        gamificacaoService.processarConclusaoSimulado(simulado);

        // Histórico geral: atualizar se já existir
        HistoricoEstudo historicoExistente = historicoEstudoRepository
                .findByUsuarioIdAndReferenciaId(usuarioId, simulado.getId())
                .orElse(null);

        if (historicoExistente != null) {
            historicoExistente.setTempoEstudoMinutos(tempoEstudoMinutos);
            historicoEstudoRepository.save(historicoExistente);
        } else {
            historicoEstudoRepository.save(
                    HistoricoEstudo.builder()
                            .tipoAtividade(TipoAtividade.SIMULADO)
                            .tempoEstudoMinutos(tempoEstudoMinutos)
                            .usuario(simulado.getUsuario())
                            .referenciaId(simulado.getId())
                            .build()
            );
        }

        return visualizarResultado(simulado.getId());
    }







    @Transactional(readOnly = true)
    public ResultadoSimuladoResponse visualizarResultado(Long simuladoId) {
        Simulado simulado = simuladoRepository.findById(simuladoId).orElseThrow(() -> new NotFoundException("Simulado não encontrado"));

        if (!StatusSimulado.FINALIZADO.equals(simulado.getStatus())) {
            throw new BusinessException("O resultado só pode ser visualizado após o simulado ser finalizado.");
        }

        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simuladoId);

        int total = questoes.size();
        int acertos = (int) questoes.stream().filter(q -> Boolean.TRUE.equals(q.getCorreta())).count();

        List<FeedbackQuestaoResponse> feedbacks = questoes.stream().map(q -> {
            var questao = q.getQuestao();

            FeedbackQuestaoResponse.FeedbackQuestaoResponseBuilder fb = FeedbackQuestaoResponse.builder().questaoId(questao.getId()).enunciado(questao.getEnunciado()).respostaCorreta(questao.getRespostaCorreta()).respostaUsuario(q.getRespostaUsuario()).correta(q.getCorreta());

            // Adiciona explicação apenas se a resposta estiver errada
            if (Boolean.FALSE.equals(q.getCorreta())) {
                fb.explicacao(questao.getExplicacao());
            }

            return fb.build();
        }).collect(Collectors.toList());

        return ResultadoSimuladoResponse.builder().simuladoId(simulado.getId()).pontuacaoFinal(simulado.getPontuacaoFinal()).totalQuestoes(total).totalAcertos(acertos).totalErros(total - acertos).feedbackQuestoes(feedbacks).build();
    }


    @Transactional
    public List<PacoteFiltroSimuladoDTO> listarFiltrosPorUsuario(Long usuarioId) {

        // 1) IDs dos pacotes ativos (leve e rápido)
        List<Long> pacoteIds = pacoteCompradoRepository
                .findPacoteIdsAtivosByUsuarioAndStatus(usuarioId, StatusCompra.APROVADA);

        if (pacoteIds.isEmpty()) return List.of();

        // 2) Headers + árvore
        List<PacoteHeaderProjection> headers = pacoteFiltroRepository.listarHeaders(pacoteIds);
        List<FiltroArvoreLinhaProjection> linhas = pacoteFiltroRepository.listarArvoreFiltros(pacoteIds);

        // 3) Bancas e níveis (distinct no banco)
        List<BancaPorPacoteProjection> bancasRows = questaoRepository.listarBancasPorPacotes(pacoteIds);
        List<NivelPorPacoteProjection> niveisRows = questaoRepository.listarNiveisPorPacotes(pacoteIds);

        Map<Long, List<String>> bancasPorPacote = new HashMap<>();
        for (BancaPorPacoteProjection r : bancasRows) {
            bancasPorPacote
                    .computeIfAbsent(r.getPacoteId(), k -> new ArrayList<>())
                    .add(r.getBanca());
        }

        Map<Long, List<String>> niveisPorPacote = new HashMap<>();
        for (NivelPorPacoteProjection r : niveisRows) {
            niveisPorPacote
                    .computeIfAbsent(r.getPacoteId(), k -> new ArrayList<>())
                    .add(r.getNivel().name()); // ✅ converte enum -> String
        }


        // 4) Cria DTOs base por pacote (LinkedHashMap mantém ordem)
        Map<Long, PacoteFiltroSimuladoDTO> pacotesMap = new LinkedHashMap<>();
        for (PacoteHeaderProjection h : headers) {
            pacotesMap.put(h.getPacoteId(),
                    PacoteFiltroSimuladoDTO.builder()
                            .pacoteId(h.getPacoteId())
                            .nomePacote(h.getNomePacote())
                            .versao(h.getVersao())
                            .concursoId(h.getConcursoId())
                            .nomeConcurso(h.getNomeConcurso())
                            .temas(new ArrayList<>())
                            .bancasDisponiveis(bancasPorPacote.getOrDefault(h.getPacoteId(), List.of()))
                            .niveisDisponiveis(niveisPorPacote.getOrDefault(h.getPacoteId(), List.of()))
                            .build()
            );
        }

        // 5) Monta árvore com caches (O(n), sem ficar varrendo lista)
        // cache: pacoteId -> temaId -> TemaFiltroDTO
        Map<Long, Map<Long, TemaFiltroDTO>> temaCache = new HashMap<>();
        // cache: (pacoteId,temaId) -> capId -> CapituloFiltroDTO
        Map<String, Map<Long, CapituloFiltroDTO>> capCache = new HashMap<>();

        for (FiltroArvoreLinhaProjection l : linhas) {
            PacoteFiltroSimuladoDTO pacoteDTO = pacotesMap.get(l.getPacoteId());
            if (pacoteDTO == null) continue;

            Map<Long, TemaFiltroDTO> temasDoPacote =
                    temaCache.computeIfAbsent(l.getPacoteId(), k -> new LinkedHashMap<>());

            TemaFiltroDTO temaDTO = temasDoPacote.get(l.getTemaId());
            if (temaDTO == null) {
                temaDTO = TemaFiltroDTO.builder()
                        .id(l.getTemaId())
                        .nome(l.getTemaNome())
                        .capitulos(new ArrayList<>())
                        .build();
                temasDoPacote.put(l.getTemaId(), temaDTO);
                pacoteDTO.getTemas().add(temaDTO);
            }

            String key = l.getPacoteId() + ":" + l.getTemaId();
            Map<Long, CapituloFiltroDTO> capsDoTema =
                    capCache.computeIfAbsent(key, k -> new LinkedHashMap<>());

            CapituloFiltroDTO capDTO = capsDoTema.get(l.getCapituloId());
            if (capDTO == null) {
                capDTO = CapituloFiltroDTO.builder()
                        .id(l.getCapituloId())
                        .nome(l.getCapituloNome())
                        .subcapitulos(new ArrayList<>())
                        .build();
                capsDoTema.put(l.getCapituloId(), capDTO);
                temaDTO.getCapitulos().add(capDTO);
            }

            // adiciona subcapítulo
            capDTO.getSubcapitulos().add(new SubcapituloFiltroDTO(
                    l.getSubcapituloId(),
                    l.getSubcapituloNome()
            ));
        }

        return new ArrayList<>(pacotesMap.values());
    }


    @Transactional
    public void deletarSimulado(Long simuladoId, Long usuarioId) {
        Simulado simulado = simuladoRepository.findById(simuladoId)
                .orElseThrow(() -> new NotFoundException("Simulado não encontrado"));

        if (!simulado.getUsuario().getId().equals(usuarioId)) {
            throw new BusinessException("Você não tem permissão para deletar este simulado");
        }

        if (simulado.getStatus() != StatusSimulado.EM_ANDAMENTO) {
            throw new BusinessException("Somente simulados em andamento podem ser deletados");
        }

        // Remove respostas vinculadas
        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findBySimuladoId(simuladoId);
        questaoSimuladoRepository.deleteAll(questoes);

        // Deleta o simulado
        simuladoRepository.delete(simulado);
    }


}

