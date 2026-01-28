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
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
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

        Usuario usuario = usuarioRepository.findById(usuarioId)
                .orElseThrow(() -> new NotFoundException("Usuário não encontrado."));

        PacoteCompradoComUsuarioDTO pacoteDTO = pacoteCompradoService.listarComprasAtivas(usuarioId).stream()
                .filter(c -> c.getConcursoId().equals(request.getConcursoId()))
                .findFirst()
                .orElseThrow(() -> new BusinessException("Usuário não possui acesso a este concurso."));

        String tituloSimulado = "Simulado " + pacoteDTO.getNomePacote();

        int quantidadeTotal = (request.getQuantidadeQuestoes() == null) ? 10 : request.getQuantidadeQuestoes();
        if (quantidadeTotal <= 0) throw new BusinessException("Quantidade de questões inválida.");

        // 0) normaliza listas vazias -> null (evita filtro estranho)
        normalizarFiltro(request);

        // 1) POOL (universo) - só IDs
        List<Long> poolIds = questaoRepository.findIdsByFiltro(request);


        if (poolIds.size() < quantidadeTotal) {
            throw new BusinessException(
                    "Pool insuficiente para montar " + quantidadeTotal +
                            ". Com esses filtros o pool retornou " + poolIds.size() + " questões."
            );
        }

        if (poolIds.isEmpty()) {
            throw new BusinessException("Não encontramos questões com os filtros selecionados. Tente ajustar os critérios.");
        }

        // Regra do negócio: se pediu 50, o filtro precisa ter 50
        if (poolIds.size() < quantidadeTotal) {
            throw new BusinessException(
                    "Pool insuficiente para montar " + quantidadeTotal + " questões. Disponível: " + poolIds.size()
            );
        }

        // 2) Preferência #1: fraquezas (até N)
        List<Long> fraquezasIds = questaoFraquezaService.buscarIdsDeFraqueza(usuario, poolIds, quantidadeTotal);

        // 3) Preferência #2: candidatos aleatórios com FOLGA (para não morrer no histórico)
        int folga = Math.min(poolIds.size(), Math.max(quantidadeTotal * 4, quantidadeTotal + 30));
        List<Long> aleatoriasIds = questaoSelectorService.selecionarIds(poolIds, folga);

        // 4) Candidatos = fraquezas primeiro + aleatórias (SEM cortar em N aqui)
        List<Long> candidatos = combinarSemRepetir(fraquezasIds, aleatoriasIds);

        // 5) Preferência #3: remover recentes (soft)
        List<Long> naoRecentes = questaoHistoricoService.filtrarIdsNaoRecentes(usuario, candidatos);

        // Se ficou pouco, completa com candidatos (inclui recentes) até a folga mínima
        naoRecentes = completarAte(naoRecentes, candidatos, Math.min(folga, poolIds.size()));

        // 6) Preferência #4: balancear por nível sobre a lista maior (não só N)
        List<Long> balanceadas = questaoBalanceService.balancearIds(naoRecentes, request, quantidadeTotal);

        // 7) Garantia final: se por qualquer motivo veio < N, completa com o pool (universo)
        balanceadas = completarAte(balanceadas, poolIds, quantidadeTotal);

        // Agora sim corta exatamente N
        List<Long> selecionadasIds = balanceadas.stream().limit(quantidadeTotal).toList();

        // 8) Busca Questao completa só das selecionadas
        List<Questao> questoesSelecionadas = questaoRepository.findByIdIn(selecionadasIds);

        // garante ordem conforme selecionadasIds
        Map<Long, Questao> porId = questoesSelecionadas.stream()
                .collect(Collectors.toMap(Questao::getId, q -> q));

        List<Questao> ordenadas = selecionadasIds.stream()
                .map(porId::get)
                .filter(Objects::nonNull)
                .toList();

        // proteção extra: se sumiu algo por integridade, completa de novo (raríssimo, mas profissional)
        if (ordenadas.size() < quantidadeTotal) {
            // completa com outras questões do pool que existam
            Set<Long> ja = ordenadas.stream().map(Questao::getId).collect(Collectors.toSet());
            List<Long> faltantes = poolIds.stream().filter(id -> !ja.contains(id)).limit(quantidadeTotal - ordenadas.size()).toList();

            if (!faltantes.isEmpty()) {
                List<Questao> extras = questaoRepository.findByIdIn(faltantes);
                ordenadas = new ArrayList<>(ordenadas);
                ordenadas.addAll(extras);
            }
        }

        if (ordenadas.size() < quantidadeTotal) {
            throw new BusinessException("Não foi possível montar o simulado com a quantidade solicitada (inconsistência de dados).");
        }

        // 9) Cria simulado
        Simulado simulado = Simulado.builder()
                .titulo(tituloSimulado)
                .usuario(usuario)
                .quantidadeQuestoes(quantidadeTotal)
                .tempoDuracao(request.getTempoDuracao())
                .dataCriacao(LocalDateTime.now(ZoneId.of("America/Sao_Paulo")))
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

        // 10) Vincula QuestaoSimulado
        AtomicInteger ordem = new AtomicInteger(1);
        List<QuestaoSimulado> vinculadas = ordenadas.stream()
                .limit(quantidadeTotal)
                .map(q -> QuestaoSimulado.builder()
                        .simulado(simulado)
                        .questao(q)
                        .pontuacaoObtida(0.0)
                        .respondida(false)
                        .ordem(ordem.getAndIncrement())
                        .build()
                ).toList();

        questaoSimuladoRepository.saveAll(vinculadas);

        return SimuladoExecucaoResponse.fromEntity(simulado, vinculadas);
    }


    private void normalizarFiltro(CriarSimuladoRequest r) {
        if (r.getTemaIds() != null && r.getTemaIds().isEmpty()) r.setTemaIds(null);
        if (r.getCapituloIds() != null && r.getCapituloIds().isEmpty()) r.setCapituloIds(null);
        if (r.getSubcapituloIds() != null && r.getSubcapituloIds().isEmpty()) r.setSubcapituloIds(null);
        if (r.getBancas() != null && r.getBancas().isEmpty()) r.setBancas(null);
        if (r.getNiveis() != null && r.getNiveis().isEmpty()) r.setNiveis(null);
    }

    private List<Long> combinarSemRepetir(List<Long> primeiro, List<Long> depois) {
        List<Long> out = new ArrayList<>();
        Set<Long> usados = new HashSet<>();
        if (primeiro != null) {
            for (Long id : primeiro) if (id != null && usados.add(id)) out.add(id);
        }
        if (depois != null) {
            for (Long id : depois) if (id != null && usados.add(id)) out.add(id);
        }
        return out;
    }

    private List<Long> completarAte(List<Long> base, List<Long> fonte, int alvo) {
        if (alvo <= 0) return List.of();
        List<Long> out = new ArrayList<>(base == null ? List.of() : base);
        Set<Long> set = new HashSet<>(out);

        if (out.size() >= alvo) return out;

        for (Long id : fonte) {
            if (out.size() >= alvo) break;
            if (id != null && set.add(id)) out.add(id);
        }
        return out;
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

        if (request == null || request.getRespostas() == null) {
            throw new BusinessException("Lista de respostas não informada");
        }

        // (Opcional) garantir que o simuladoId do body bate com o path
        if (request.getSimuladoId() != null && !request.getSimuladoId().equals(simuladoId)) {
            throw new BusinessException("simuladoId do body difere do simuladoId da URL");
        }

        // 1) ids que chegaram
        List<Long> questaoIds = request.getRespostas().stream()
                .map(RespostaQuestaoSimulado::getQuestaoId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        if (questaoIds.isEmpty()) {
            throw new BusinessException("Nenhuma questão informada");
        }

        // 2) busca tudo em lote JÁ trazendo Questao (mata N+1)
        List<QuestaoSimulado> registros = questaoSimuladoRepository
                .findWithQuestaoBySimulado_IdAndQuestao_IdIn(simuladoId, questaoIds);

        // valida se veio tudo (evita resposta pra questão que não pertence ao simulado)
        if (registros.size() != questaoIds.size()) {
            Set<Long> encontrados = registros.stream()
                    .map(qs -> qs.getQuestao().getId())
                    .collect(Collectors.toSet());

            List<Long> faltando = questaoIds.stream()
                    .filter(id -> !encontrados.contains(id))
                    .toList();

            throw new NotFoundException("Questões não encontradas no simulado: " + faltando);
        }

        // Map questaoId -> QuestaoSimulado
        Map<Long, QuestaoSimulado> porQuestaoId = registros.stream()
                .collect(Collectors.toMap(qs -> qs.getQuestao().getId(), Function.identity()));

        // 3) aplica as respostas
        for (RespostaQuestaoSimulado r : request.getRespostas()) {
            Long qId = r.getQuestaoId();
            if (qId == null) continue;

            QuestaoSimulado qs = porQuestaoId.get(qId);
            if (qs == null) {
                throw new NotFoundException("Questão não encontrada: " + qId);
            }

            String resp = r.getRespostaUsuario();
            resp = (resp == null) ? null : resp.trim();

            // ✅ não respondida
            if (resp == null || resp.isEmpty()) {
                qs.setRespostaUsuario(null);
                qs.setRespondida(false);
                qs.setCorreta(null); // mantém semântico com seu modelo
                qs.setPontuacaoObtida(0.0);
                continue;
            }

            // ✅ valida alternativa (A/B/C/D)
            String respUpper = resp.toUpperCase(Locale.ROOT);
            boolean alternativaValida = respUpper.equals("A")
                    || respUpper.equals("B")
                    || respUpper.equals("C")
                    || respUpper.equals("D");

            if (!alternativaValida) {
                throw new BusinessException("Resposta inválida para questaoId=" + qId + ": " + resp);
            }

            boolean correta = qs.getQuestao().getRespostaCorreta().equalsIgnoreCase(respUpper);

            qs.setRespostaUsuario(respUpper);
            qs.setRespondida(true);
            qs.setCorreta(correta);
            qs.setPontuacaoObtida(correta ? 1.0 : 0.0);
        }

        // 4) salva tudo de uma vez
        questaoSimuladoRepository.saveAll(registros);

        // 5) finaliza simulado
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

        // ✅ puxa árvore inteira (evita N+1 de tema/subcap)
        List<QuestaoSimulado> questoes = questaoSimuladoRepository.findWithArvoreBySimulado_Id(simulado.getId());

        List<QuestaoHistoricoUsuario> historicos = new ArrayList<>(questoes.size());
        int acertos = 0;

        // 1) Normaliza não respondidas e monta histórico + lista de erradas respondidas
        List<Long> erradasRespondidas = new ArrayList<>();

        for (QuestaoSimulado qs : questoes) {
            boolean foiRespondida = Boolean.TRUE.equals(qs.getRespondida());

            if (!foiRespondida) {
                qs.setRespondida(false);
                qs.setCorreta(null);
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

            if (foiRespondida && !correta) {
                erradasRespondidas.add(qs.getQuestao().getId());
            }
        }

        // 2) cartões existentes (1 query)
        Set<Long> jaTemCartao = erradasRespondidas.isEmpty()
                ? Set.of()
                : new HashSet<>(cartaoRepository.findQuestaoIdsQueJaTemCartao(usuarioId, erradasRespondidas));

        // 3) cache baralho por tema
        Map<Long, Baralho> baralhoPorTemaId = new HashMap<>();

        // 4) cria cartões só para erradas respondidas sem cartão
        for (QuestaoSimulado qs : questoes) {
            boolean foiRespondida = Boolean.TRUE.equals(qs.getRespondida());
            boolean correta = Boolean.TRUE.equals(qs.getCorreta());

            if (!(foiRespondida && !correta)) continue;

            Long questaoId = qs.getQuestao().getId();
            if (jaTemCartao.contains(questaoId)) continue;

            Tema tema = qs.getQuestao().getSubcapitulo().getCapitulo().getTema();
            Long temaId = tema.getId();

            Baralho baralho = baralhoPorTemaId.computeIfAbsent(temaId, id -> {
                return baralhoRepository.findByUsuarioIdAndTemaId(usuarioId, id)
                        .orElseGet(() -> baralhoRepository.save(
                                Baralho.builder()
                                        .titulo("Erros em " + tema.getNome())
                                        .tema(tema)
                                        .usuario(simulado.getUsuario())
                                        .build()
                        ));
            });

            cartaoRepository.save(
                    Cartao.builder()
                            .frente(qs.getQuestao().getEnunciado())
                            .verso(qs.getQuestao().getExplicacao())
                            .tema(tema)
                            .usuario(simulado.getUsuario())
                            .questao(qs.getQuestao())
                            .baralho(baralho)
                            .precisaRevisar(true)
                            .build()
            );
        }

        // persiste
        questaoSimuladoRepository.saveAll(questoes);
        questaoHistoricoUsuarioRepository.saveAll(historicos);

        double pontuacaoFinal = (acertos * 100.0) / questoes.size();

        long tempoEstudoMinutos;
        if (porTempo && simulado.getTempoDuracao() != null) {
            tempoEstudoMinutos = simulado.getTempoDuracao();
        } else {
            tempoEstudoMinutos = Math.max(0, Duration.between(simulado.getDataCriacao(), fimSimulado).toMinutes());
            if (simulado.getTempoDuracao() != null) {
                tempoEstudoMinutos = Math.min(tempoEstudoMinutos, simulado.getTempoDuracao());
            }
        }

        simulado.setStatus(StatusSimulado.FINALIZADO);
        simulado.setPontuacaoFinal(pontuacaoFinal);
        simulado.setDataFinalizacao(fimSimulado);
        simuladoRepository.save(simulado);

        gamificacaoService.processarConclusaoSimulado(simulado);

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


    // filtro da tela de montar o simulado
    @Transactional(readOnly = true)
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

        Map<Long, Set<String>> bancasPorPacote = new HashMap<>();

        for (BancaPorPacoteProjection r : bancasRows) {
            bancasPorPacote
                    .computeIfAbsent(r.getPacoteId(), k -> new LinkedHashSet<>())
                    .add(r.getBanca());
        }


        Map<Long, Set<String>> niveisPorPacote = new HashMap<>();

        for (NivelPorPacoteProjection r : niveisRows) {
            niveisPorPacote
                    .computeIfAbsent(r.getPacoteId(), k -> new LinkedHashSet<>())
                    .add(r.getNivel().name());
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
                            .bancasDisponiveis(
                                    new ArrayList<>(bancasPorPacote.getOrDefault(
                                            h.getPacoteId(),
                                            Collections.emptySet()
                                    ))
                            )
                            .niveisDisponiveis(
                                    new ArrayList<>(niveisPorPacote.getOrDefault(
                                            h.getPacoteId(),
                                            Collections.emptySet()
                                    ))
                            ).build()

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

