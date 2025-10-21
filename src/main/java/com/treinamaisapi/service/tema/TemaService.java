package com.treinamaisapi.service.tema;

import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.repository.TemaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class TemaService {

    private final TemaRepository temaRepository;
    public TemaResponse criar (TemaRequest request) {
        var tema = Tema.builder().nome(request.getNome()).build();
        temaRepository.save(tema);
        return new TemaResponse( tema.getId(), tema.getNome());
    }

    public List<TemaResponse> listar () {
        return temaRepository.findAll().stream().map(
                t -> new TemaResponse(t.getId(), t.getNome())
        ).toList();
    }

    }


