package com.treinamaisapi.service.compra.pacote;

import com.treinamaisapi.common.dto.pacote.request.PacoteRequest;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PacoteService {

    private final PacoteRepository pacoteRepository;
    private final ConcursoRepository concursoRepository;
    private final TemaRepository temaRepository;

    @Transactional
    public PacoteResponse criarPacote(PacoteRequest request) {
        Concurso concurso = concursoRepository.findById(request.getConcursoId())
                .orElseThrow(() -> new IllegalArgumentException("Concurso não encontrado."));

        if (pacoteRepository.existsByNomeAndConcursoId(request.getNome(), concurso.getId())) {
            throw new IllegalStateException("Já existe um pacote com esse nome neste concurso.");
        }

        List<Tema> temas = temaRepository.findAllById(request.getTemaIds());
        if (temas.isEmpty()) {
            throw new IllegalArgumentException("Nenhum tema válido foi informado.");
        }

        Pacote pacote = Pacote.builder()
                .nome(request.getNome())
                .descricao(request.getDescricao())
                .preco(request.getPreco())
                .duracaoDias(request.getDuracaoDias())
                .concurso(concurso)
                .temas(temas)
                .build();

        pacoteRepository.save(pacote);

        return PacoteResponse.builder()
                .id(pacote.getId())
                .nome(pacote.getNome())
                .descricao(pacote.getDescricao())
                .preco(pacote.getPreco())
                .duracaoDias(pacote.getDuracaoDias())
                .concursoNome(concurso.getNome())
                .temas(temas.stream().map(Tema::getNome).collect(Collectors.toList()))
                .build();
    }
}
