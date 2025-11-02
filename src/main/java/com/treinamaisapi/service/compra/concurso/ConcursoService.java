package com.treinamaisapi.service.compra.concurso;

import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.entity.Concurso;
import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.repository.ConcursoRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ConcursoService {

    private final ConcursoRepository concursoRepository;

    @Transactional
    public ConcursoResponse criarConcurso(ConcursoRequest request) {
        concursoRepository.findByNomeIgnoreCase(request.getNome()).ifPresent(c -> {
            throw new IllegalStateException("Já existe um concurso com esse nome.");
        });

        Concurso concurso = Concurso.builder()
                .nome(request.getNome())
                .descricao(request.getDescricao())
                .dataProva(request.getDataProva())
                .build();

        concursoRepository.save(concurso);

        return ConcursoResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .descricao(concurso.getDescricao())
                .dataProva(concurso.getDataProva())
                .pacotes(concurso.getPacotes() == null ? null :
                        concurso.getPacotes().stream()
                                .map(p -> p.getNome())
                                .collect(Collectors.toList()))
                .build();

    }

    @Transactional(readOnly = true)
    public ConcursoResponse buscarPorId(Long id) {
        Concurso concurso = concursoRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Concurso não encontrado."));

        return ConcursoResponse.builder()
                .id(concurso.getId())
                .nome(concurso.getNome())
                .descricao(concurso.getDescricao())
                .dataProva(concurso.getDataProva())
                .pacotes(concurso.getPacotes().stream()
                        .map(Pacote::getNome)
                        .collect(Collectors.toList()))
                .build();
    }
}
