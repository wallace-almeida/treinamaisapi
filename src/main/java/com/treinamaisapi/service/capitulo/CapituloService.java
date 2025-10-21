package com.treinamaisapi.service.capitulo;

import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.entity.capitulo.Capitulo;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.repository.CapituloRepository;

import com.treinamaisapi.repository.TemaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CapituloService {

    private final CapituloRepository capituloRepository;
    private final TemaRepository temaRepository;

    public CapituloResponse criar(CapituloRequest request) {
        var tema = temaRepository.findById(
                request.getTemaId()).orElseThrow(() -> new IllegalArgumentException("Tema não encontrado"));

        var capitulo = Capitulo.builder().nome(request.getNome()).tema(tema).build();
        capituloRepository.save(capitulo);
        return new CapituloResponse(capitulo.getId(), capitulo.getNome(), tema.getNome());
    }

    public List<TemaResponse> listar() {
        return temaRepository.findAll().stream().map(
                t -> new TemaResponse(t.getId(), t.getNome())
        ).toList();
    }

    public List<CapituloResponse> listarPorTema(Long temaId) {
        return capituloRepository.findAll().stream()
                .filter(c -> c.getTema().getId().equals(temaId))
                .map(c -> new CapituloResponse(c.getId(), c.getNome(), c.getTema().getNome()))
                .toList();
    }

}


